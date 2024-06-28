open! Core

(* module NodeId = struct module T = struct type t = { row : int ; col : int
   } [@@deriving compare, sexp] end

   include Comparable.Make (T) include T end

   module NodeType = struct type t = | Regular | Start | End [@@deriving
   compare, sexp, equal] end

   module Node = struct module T = struct type t = { id : NodeId.t ; mutable
   neighbors : NodeId.t list ; node_type : NodeType.t } [@@deriving compare,
   sexp, equal] end

   include Comparable.Make (T) include T end

   module Maze = struct type t = { maze : Node.t NodeId.Map.t ; start_node :
   Node.t option } end *)

(* let get_neighbors (node_id : NodeId.t) : NodeId.t list = [ { NodeId.row =
   node_id.row + 1; col = node_id.col } ; { NodeId.row = node_id.row - 1; col
   = node_id.col } ; { NodeId.row = node_id.row; col = node_id.col + 1 } ; {
   NodeId.row = node_id.row; col = node_id.col - 1 } ] ;; *)

let get_neighbors (row, col) (maze : char list list) =
  let initial_list =
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
  in
  List.filter initial_list ~f:(fun (row, col) ->
    match List.nth maze row with
    | Some row ->
      (match List.nth row col with Some _ -> true | None -> false)
    | None -> false)
;;

let create_map lines = List.map lines ~f:(fun line -> String.to_list line)

(* let _create_map lines = let coords_set : Maze.t = { Maze.maze =
   NodeId.Map.empty; start_node = None } in let final_map = List.foldi lines
   ~init:coords_set ~f:(fun row map line -> String.foldi line ~init:map
   ~f:(fun col map char -> let coords = { NodeId.row; col } in let node_type
   = match char with | '.' -> Some NodeType.Regular | 'S' -> Some
   NodeType.Start | 'E' -> Some NodeType.End | _ -> None in match node_type
   with | Some node_type -> let new_node = { Node.id = coords; neighbors =
   []; node_type } in let new_maze = Map.add_exn map.maze ~key:coords
   ~data:new_node in (match node_type with | NodeType.Start -> {
   Maze.start_node = Some new_node; maze = new_maze } | _ -> {
   Maze.start_node = map.start_node; maze = new_maze }) | None -> map)) in
   Map.iter final_map.maze ~f:(fun node -> node.neighbors <- List.filter_map
   (get_neighbors node.id) ~f:(fun neighbor -> match Map.mem final_map.maze
   neighbor with | true -> Some neighbor | false -> None)); final_map ;; *)

let find_start maze =
  List.find_mapi maze ~f:(fun row_number row ->
    List.find_mapi row ~f:(fun col_number chr ->
      match chr with 'S' -> Some (row_number, col_number) | _ -> None))
;;

let rec find_path
  (maze : char list list)
  (current : int * int)
  (path_so_far : (int * int) list)
  =
  let path_so_far = path_so_far @ [ current ] in
  let neighbors =
    List.filter_map (get_neighbors current maze) ~f:(fun (row, col) ->
      let was_explored =
        List.mem path_so_far (row, col) ~equal:[%equal: int * int]
      in
      let chr = List.nth_exn (List.nth_exn maze row) col in
      match was_explored, chr with
      | _, '#' -> None
      | false, chr -> Some ((row, col), chr)
      | _, _ -> None)
  in
  let next_path =
    List.find_map neighbors ~f:(fun ((row, col), chr) ->
      match chr with
      | '.' | 'S' -> find_path maze (row, col) path_so_far
      | 'E' -> Some [ row, col ]
      | _ -> None)
  in
  match next_path with
  | Some next_path -> Some (List.append [ current ] next_path)
  | None -> None
;;

(* List.iter neighbors ~f:(fun (row, col) -> path_so_far @ find_path2 maze
   (row, col) explored path_so_far) *)

(* (List.nth_exn (List.nth_exn maze row) col *)

(* in List.iter neighbors ~f:(fun neighbor -> match neighbor with |) *)

(* let rec _find_path (current : Node.t) (map : Maze.t) (path_so_far : Node.t
   list) : Node.t list = let path_so_far = current :: path_so_far in
   List.concat (List.filter_map current.neighbors ~f:(fun neighbor_coord ->
   let neighbor_node = Map.find_exn map.maze neighbor_coord in match List.mem
   path_so_far neighbor_node ~equal:Node.equal with | true -> None | false ->
   Some (match neighbor_node.node_type with | Regular | Start -> path_so_far
   @ _find_path neighbor_node map path_so_far | End -> [ neighbor_node ])))
   ;; *)

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let lines = In_channel.read_lines (File_path.to_string input_file) in
        let maze = create_map lines in
        let start_node = find_start maze in
        match start_node with
        | Some start_node ->
          let path = find_path maze start_node [] in
          (match path with
           | None -> print_s [%message "maze error"]
           | Some path ->
             List.iter path ~f:(fun (row, col) ->
               print_s [%message (Int.to_string row) (Int.to_string col)]))
        | None -> print_s [%message "no value"]
      (* let maze = create_map lines in match maze.start_node with | Some
         start_node -> find_path start_node maze [] |> List.iter ~f:(fun
         value -> print_s [%message (value : Node.t)]) | None -> print_s
         [%message "no value"] *)

      (* Map.iter maze ~f:(fun value -> print_s [%message (value :
         Node.t)]) *)

      (* List.concat_map ~f:(fun line -> String. line ~f:(fun char ->
         String.of_char char)) *)]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
