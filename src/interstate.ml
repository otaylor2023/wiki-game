open! Core

module City = struct
  type t = string [@@deriving compare, sexp, hash]

  let of_string s =
    String.strip s |> String.split_on_chars ~on:[ '.'; ' ' ] |> String.concat
  ;;

  (* let no_period = String.drop_suffix (List.hd_exn str_list) 1 in
     String.concat ([ no_period ] @ List.drop str_list 1) *)

  let equal = String.equal
end

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Interstate = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Highway = struct
    module T = struct
      type t =
        { cities : City.t * City.t
        ; name : string
        }
      [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    (* let of_string s = match String.split s ~on:',' with | name :: cities
       -> Some { T.name; cities } | [] -> None ;; *)
  end

  type t = Highway.Set.t [@@deriving sexp_of]

  let create_highway_list (s : string) : Highway.T.t list option =
    match String.split s ~on:',' with
    | name :: cities ->
      Some
        (List.concat
           (List.mapi cities ~f:(fun index city1 ->
              List.map
                (List.drop cities (index + 1))
                ~f:(fun city2 ->
                  { Highway.T.name
                  ; cities = City.of_string city1, City.of_string city2
                  }))))
    | [] -> None
  ;;

  (* let final_list = List.iteri ((cities : string list) ~f:(fun index (city1
     : string) -> List.iter (List.drop cities (index + 1)) ~f:(fun city2 ->
     (highway_list @ [{name; city1, city2}])))) *)

  let of_file input_file : t =
    let highways =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match create_highway_list s with
        | Some highway_list -> highway_list
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    Highway.Set.of_list highways
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let interstate = Interstate.of_file input_file in
        printf !"%{sexp: Interstate.t}\n" interstate]
;;

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G = Graph.Imperative.Graph.Concrete (City)

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
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let interstate = Interstate.of_file input_file in
        let graph = G.create () in
        Set.iter interstate ~f:(fun { name = _; cities = city1, city2 } ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* List.iteri cities ~f:(fun index city1 -> List.iter (List.drop cities
   (index + 1)) ~f:(fun city2 -> G.add_edge graph city1 city2))); *)

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
