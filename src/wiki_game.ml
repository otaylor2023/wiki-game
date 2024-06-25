open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
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
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href*='/wiki/']"
  |> to_list
  |> List.filter_map ~f:(fun a -> match Wikipedia_namespace.namespace (R.attribute "href" a) with 
  | None -> Some (R.attribute "href" a)
  | _ -> None)
  |> List.dedup_and_sort ~compare:String.compare (*TODO: optimize *)
  (* (String.is_prefix (R.attribute "href" a) ~prefix:"/wiki/"), *)
;;

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
  type t = {title: string
  ; url: string}
  [@@deriving compare, sexp, hash, equal]

  let of_string url =
    let title = String.split_on_chars url ~on:['/']
    |> List.last_exn
    |> String.substr_replace_all ~pattern:"(" ~with_:""
    |> String.substr_replace_all ~pattern:")" ~with_:""
   in
    {title = title; url = url}
  ;;
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
    let vertex_attributes (v : Article.t) = [ `Shape `Box; `Label v.title; `Fillcolor 1000 ]
    let vertex_name (v : Article.t) = v.title
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* let rec visualize_rec (depth : int) (current_article : Article.t option) ~graph ~how_to_fetch : Article.t option =
  match Int.equal depth 0, current_article with
  | true, Some current_article -> Some current_article
  | _, None -> None
  | false, Some current_article ->
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:current_article.url in
  let child_links = get_linked_articles contents in
  match List.is_empty child_links with
  | true -> Some current_article
  | false -> List.iter child_links ~f:(fun child_link ->
    let child_article = (visualize_rec (depth-1) (Some (Article.of_string child_link)) ~graph ~how_to_fetch) in
    match child_article with 
    | None -> None
    | Some article -> G.add_edge graph current_article article); None
;; *)

let rec visualize_rec (depth : int) ~(current_article : Article.t) ~(parent_article : Article.t option) ~graph ~how_to_fetch =
  if depth = 0 then () else
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:current_article.url in
  let child_links = get_linked_articles contents in
  List.iter child_links ~f:(fun child_link ->
    let child_article = (Article.of_string child_link) in
    (visualize_rec (depth-1) ~current_article:child_article ~parent_article:(Some current_article) ~graph ~how_to_fetch));
    match parent_article with
    | Some parent_article -> G.add_edge graph current_article parent_article
    | None -> ()

    (* G.add_edge graph current_article article *)

;;

(*   | false -> List.iter child_links ~f:(fun child_link ->
    let child_article = (visualize_rec (depth-1) (Article.of_string child_link) ~graph ~how_to_fetch) in
    match child_article with 
    | Some article -> G.add_edge graph current_article article)
    | None -> None *)

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () =
  let graph = G.create () in
  let origin_article = Article.of_string origin in
  visualize_rec max_depth ~current_article:origin_article ~parent_article:None ~graph ~how_to_fetch;
  Dot.output_graph
  (Out_channel.create (File_path.to_string output_file))
  graph;


;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
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

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
