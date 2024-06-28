open! Core

(** [get_linked_articles] should return a list of wikipedia article lengths contained in
    the input.

    Note that [get_linked_articles] should ONLY return things that look like wikipedia
    articles. In particular, we should discard links that are:
    - Wikipedia pages under special namespaces that are not articles (see
      https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
    - other Wikipedia internal URLs that are not articles
    - resources that are external to Wikipedia
    - page headers

    One nice think about Wikipedia is that stringent content moderation results in
    uniformity in article format. We can expect that all Wikipedia article links parsed
    from a Wikipedia page will have the form "/wiki/<TITLE>". **)

let _get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href^='/wiki/']"
  |> to_list
  |> List.filter_map ~f:(fun a ->
    match Wikipedia_namespace.namespace (R.attribute "href" a) with
    | None -> Some (R.attribute "href" a)
    | _ -> None)
  |> String.Hash_set.of_list
  |> Hash_set.to_list
;;

let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href^='/wiki/']"
  |> to_list
  |> List.filter_map ~f:(fun a ->
    match Wikipedia_namespace.namespace (R.attribute "href" a) with
    | None -> Some (R.attribute "href" a)
    | _ -> None)
  |> List.dedup_and_sort ~compare:String.compare
;;

(* List.dedup_and_sort ~compare:String.compare *)

(*TODO: optimize *)
(* (String.is_prefix (R.attribute "href" a) ~prefix:"/wiki/"), *)

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module Article = struct
  module T = struct
    type t =
      { title : string
      ; url : string
      ; path : string list
      }
    [@@deriving compare, sexp]

    let equal (t1 : t) (t2 : t) = String.equal t1.url t2.url
    let hash (t1 : t) = String.hash t1.url

    let hash_fold_t (state : Ppx_hash_lib.Std.Hash.state) (t1 : t) =
      String.hash_fold_t state t1.url
    ;;
  end

  include Hashable.Make (T)
  include T

  let of_string ~url ~path =
    let title =
      String.split_on_chars url ~on:[ '/' ]
      |> List.last_exn
      |> String.substr_replace_all ~pattern:"(" ~with_:""
      |> String.substr_replace_all ~pattern:")" ~with_:""
    in
    { title; url; path }
  ;;

  let url_equal t1 t2 = String.equal t1.url t2.url
end

module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_attributes (v : Article.t) =
      [ `Shape `Box; `Label v.title; `Fillcolor 1000 ]
    ;;

    let vertex_name (v : Article.t) = v.title
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* let rec visualize_rec (depth : int) (current_article : Article.t option)
   ~graph ~how_to_fetch : Article.t option = match Int.equal depth 0,
   current_article with | true, Some current_article -> Some current_article
   | _, None -> None | false, Some current_article -> let contents =
   File_fetcher.fetch_exn how_to_fetch ~resource:current_article.url in let
   child_links = get_linked_articles contents in match List.is_empty
   child_links with | true -> Some current_article | false -> List.iter
   child_links ~f:(fun child_link -> let child_article = (visualize_rec
   (depth-1) (Some (Article.of_string child_link)) ~graph ~how_to_fetch) in
   match child_article with | None -> None | Some article -> G.add_edge graph
   current_article article); None ;; *)

let rec visualize_rec
  (depth : int)
  ~(current_article : Article.t)
  ~(parent_article : Article.t option)
  ~graph
  ~how_to_fetch
  =
  if depth = 0
  then ()
  else (
    let contents =
      File_fetcher.fetch_exn how_to_fetch ~resource:current_article.url
    in
    let child_links = get_linked_articles contents in
    List.iter child_links ~f:(fun child_link ->
      let child_article = Article.of_string ~url:child_link ~path:[] in
      visualize_rec
        (depth - 1)
        ~current_article:child_article
        ~parent_article:(Some current_article)
        ~graph
        ~how_to_fetch);
    match parent_article with
    | Some parent_article -> G.add_edge graph current_article parent_article
    | None -> ())
;;

(* G.add_edge graph current_article article *)

(* | false -> List.iter child_links ~f:(fun child_link -> let child_article =
   (visualize_rec (depth-1) (Article.of_string child_link) ~graph
   ~how_to_fetch) in match child_article with | Some article -> G.add_edge
   graph current_article article) | None -> None *)

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () =
  let graph = G.create () in
  let origin_article = Article.of_string ~url:origin ~path:[] in
  visualize_rec
    max_depth
    ~current_article:origin_article
    ~parent_article:None
    ~graph
    ~how_to_fetch;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let is_https_pref (url : string) : bool =
  String.is_prefix url ~prefix:"https"
;;

let add_child_articles
  (current_set : Article.Hash_set.t)
  (article : Article.t)
  ~how_to_fetch
  (ignore_list : string Hash_set.t)
  =
  let is_parent_external = is_https_pref article.url in
  File_fetcher.fetch_exn how_to_fetch ~resource:article.url
  |> get_linked_articles
  |> List.iter ~f:(fun child_link ->
    match not (Hash_set.mem ignore_list child_link) with
    | false -> ()
    | true ->
      let child_url =
        match is_parent_external with
        | false -> child_link
        | true ->
          (match is_https_pref child_link with
           | true -> child_link
           | false -> String.append "https://en.wikipedia.org" child_link)
      in
      let child_article =
        Article.of_string
          ~url:child_url
          ~path:(List.append [ child_url ] article.path)
      in
      Hash_set.add current_set child_article)
;;

let get_child_articles
  (article : Article.t)
  ~how_to_fetch
  (ignore_list : string Hash_set.t option)
  : Article.Hash_set.t
  =
  let child_articles = Article.Hash_set.create ~growth_allowed:true () in
  let is_parent_external = is_https_pref article.url in
  File_fetcher.fetch_exn how_to_fetch ~resource:article.url
  |> get_linked_articles
  |> List.iter ~f:(fun child_link ->
    let get_child_url link =
      match is_parent_external with
      | false -> link
      | true ->
        (match is_https_pref link with
         | true -> link
         | false -> String.append "https://en.wikipedia.org" link)
    in
    let child_url =
      match ignore_list with
      | None -> Some (get_child_url child_link)
      | Some ignore_list ->
        (match not (Hash_set.mem ignore_list child_link) with
         | false -> None
         | true -> Some (get_child_url child_link))
    in
    match child_url with
    | Some child_url ->
      Article.of_string
        ~url:child_url
        ~path:(List.append [ child_url ] article.path)
      |> Hash_set.add child_articles
    | None -> ());
  child_articles
;;

(* |> List.map ~f:(fun child_link -> let child_url = match is_parent_external
   with | false -> child_link | true -> (match is_https_pref child_link with
   | true -> child_link | false -> String.append "https://en.wikipedia.org"
   child_link) in Article.of_string ~url:child_url ~path:(List.append [
   child_url ] article.path)) *)

let rec find_path_bfs
  ~(nth_degree_articles : Article.Hash_set.t)
  ~(destination : Article.T.t)
  ~(explored_articles : string Hash_set.t)
  ~(* TODO: convert to set *)
  (depth_left : int)
  ~how_to_fetch
  : string list option
  =
  (* let was_found = Hash_set.mem nth_degree_articles destination in match
     was_found, Int.equal depth_left 0 with | _, true -> None | true, _ ->
     let found_dest = Hash_set.find nth_degree_articles ~f:(fun article ->
     Article.url_equal article destination) in (match found_dest with | Some
     found_dest -> Some found_dest.path | None -> None) | false, _ -> *)
  (* print_s [%message (Hash_set.length nth_degree_articles : int)]; *)
  (* let time1 = Time_float.now () in *)
  let found =
    Hash_set.find nth_degree_articles ~f:(fun article ->
      Article.url_equal article destination)
  in
  (* let time2 = Time_float.now () in *)
  (* let () = print_s [%message (Time_float.diff time2 time1 :
     Time_float.Span.t)] in *)
  match found, Int.equal depth_left 0 with
  | Some found_dest, _ -> Some found_dest.path
  | _, true -> None
  | _, _ ->
    (* return some [final value] or recurse, fold until fold child articles
       into new explored list stop if list length is 1*)
    (* make list of next level of child articles *)
    let next_degree_articles =
      Article.Hash_set.create ~growth_allowed:true ~size:10500 ()
    in
    let () =
      Hash_set.iter nth_degree_articles ~f:(fun article ->
        (* let time_now = Time_now.nanoseconds_since_unix_epoch () in let ()
           = print_s [%message (time_now : Int63.t)] in *)
        add_child_articles
          next_degree_articles
          article
          ~how_to_fetch
          explored_articles)
    in
    (* List.concat_map nth_degree_articles ~f:(fun article -> let children =
       get_child_articles article ~how_to_fetch in List.filter children
       ~f:(fun child -> match not (Hash_set.mem explored_articles child.url)
       with | true -> Hash_set.add explored_articles child.url; true | false
       -> false)) *)
    (* let new_explored = List.append explored_articles next_degree_articles
       in *)
    find_path_bfs
      ~nth_degree_articles:next_degree_articles
      ~destination
      ~explored_articles
      ~depth_left:(depth_left - 1)
      ~how_to_fetch
;;

(* List.fold_until nth_degree_articles ~init:explored_articles ~finish:(fun
   explored_articles -> explored_articles) ~f:(fun explored_articles article
   -> match Article.url_equal article destination with | true -> Stop [
   article ] | false -> let check_explored = (List.mem explored_articles
   ~equal:Article.url_equal) in let filtered_child_articles =
   (get_child_articles article ~how_to_fetch) |> (List.filter
   ~f:check_explored) in Continue (List.append explored_articles
   filtered_child_articles)) in

   match Article.url_equal (List.hd_exn next_degree_articles) destination
   with | *)
(* Continue (List.append explored_articles (List.filter (get_child_articles
   article ~how_to_fetch) ~f:(fun child_article -> List.mem explored_articles
   child_article ~equal:Article.url_equal)))) *)

(* in match Article.equal (List.hd_exn next_degree_articles) with | -> ) *)

(* (get_child_articles article ~how_to_fetch) |> List.filter ~f:(fun
   child_article -> List.mem explored_articles child_article
   ~equal:Article.url_equal)) |> List.append explored_articles)) *)

(* let rec find_path_rec ~(current_article : Article.t) ~(destination :
   Article.t) ~(path_so_far : Article.t list) ~(depth_left : int)
   ~how_to_fetch : Article.t list option = if Int.equal depth_left 0 then
   None else ( let new_path = path_so_far @ [ current_article ] in let
   child_articles = get_child_articles current_article ~how_to_fetch in let
   () = print_s [%message (current_article : Article.t) (List.length
   child_articles : int)] in let rest_of_path = List.find_map child_articles
   ~f:(fun child_article -> match ( List.mem path_so_far child_article
   ~equal:Article.equal , Article.equal child_article destination ) with | _,
   true -> Some [ child_article ] | false, false -> find_path_rec
   ~current_article:child_article ~destination ~path_so_far:new_path
   ~depth_left:(depth_left - 1) ~how_to_fetch | _, _ -> None) in match
   rest_of_path with | Some rest_of_path -> Some (List.append [
   current_article ] rest_of_path) | None -> None) ;; *)

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
(* let _find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
   let origin_article = Article.of_string ~url:origin ~path:[] in let
   destination_article = Article.of_string ~url:destination ~path:[] in
   find_path_rec ~current_article:origin_article
   ~destination:destination_article ~path_so_far:[] ~depth_left:max_depth
   ~how_to_fetch ;; *)

(* let run_bfs (n : int) (child_articles : Article.t list)
   (destination_article : Article.t) ~how_to_fetch (max_depth : int) = match
   find_path_bfs ~nth_degree_articles:child_articles
   ~destination:destination_article ~explored_articles:
   (String.Hash_set.create ~growth_allowed:true ~size:500 ()) ~how_to_fetch
   ~depth_left:max_depth with | None -> print_endline "No path found!"; [] |
   Some trace -> trace ;; *)

(* let end_time = Time_now.nanoseconds_since_unix_epoch () in let time_taken
   = Base.Int63.( - ) end_time start_time in print_s [%message (trace :
   string list) ~time_taken: (Float.( / ) (Float.of_int63 time_taken)
   (Float.of_int 1000000000) : Float.t)]] *)

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        let start_time = Time_now.nanoseconds_since_unix_epoch () in
        let origin_article =
          Article.of_string ~url:origin ~path:[ origin ]
        in
        let child_articles =
          get_child_articles origin_article ~how_to_fetch None
        in
        let destination_article =
          Article.of_string ~url:destination ~path:[]
        in
        match
          find_path_bfs
            ~nth_degree_articles:child_articles
            ~destination:destination_article
            ~explored_articles:
              (String.Hash_set.create ~growth_allowed:true ~size:500 ())
            ~how_to_fetch
            ~depth_left:max_depth
        with
        | None -> print_endline "No path found!"
        | Some trace ->
          let end_time = Time_now.nanoseconds_since_unix_epoch () in
          let time_taken_int = Base.Int63.( - ) end_time start_time in
          let time_taken_float =
            Float.( / )
              (Float.of_int63 time_taken_int)
              (Float.of_int 1000000000)
          in
          print_s
            [%message
              (trace : string list) ~time_taken:(time_taken_float : Float.t)]]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
