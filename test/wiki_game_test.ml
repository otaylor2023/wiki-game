open! Core
open! Expect_test_helpers_core
open! Wiki_game_lib

let%expect_test "get_first_item_of_all_unordered_lists" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter
    (Lambda_soup_utilities.get_first_item_of_all_unordered_lists contents)
    ~f:print_endline;
  [%expect
    {|
    All feliforms, such as domestic cats, big cats, hyenas, mongooses, civets
    All birds of prey, such as hawks, eagles, falcons and owls
    |}]
;;

let%expect_test "get_first_item_of_second_unordered_list" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  print_endline
    (Lambda_soup_utilities.get_first_item_of_second_unordered_list contents);
  [%expect
    {|
    All birds of prey, such as hawks, eagles, falcons and owls
    |}]
;;

let%expect_test "get_bolded_text" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (Lambda_soup_utilities.get_bolded_text contents) ~f:print_endline;
  [%expect
    {|
  carnivore
  Predators
  Scavengers
  insectivores
  piscivores
  |}]
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (Wiki_game.get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
  /wiki/Animal
  /wiki/Caniformia
  /wiki/Feliformia
   |}]
;;

let%expect_test "get_credits" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      File_fetcher.How_to_fetch.Remote
      ~resource:"https://www.imdb.com/name/nm0000706/?ref_=fn_al_nm_1"
  in
  List.iter (Imdb.get_credits contents) ~f:print_endline;
  [%expect
    {|
  Everything Everywhere All at Once    
  Crouching Tiger, Hidden Dragon
  Crazy Rich Asians
  Tomorrow Never Dies
   |}]
;;
