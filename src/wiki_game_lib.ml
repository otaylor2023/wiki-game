open! Core
module File_fetcher = File_fetcher
module Lambda_soup_utilities = Lambda_soup_utilities
module Wiki_game = Wiki_game
module Imdb = Imdb

let command =
  Command.group
    ~summary:"A tool for playing the wikipedia game, and other utilities"
    [ "wiki-game", Wiki_game.command
    ; "interstate", Interstate.command
    ; "social-network", Social_network.command
    ; "lambda-soup-utilities", Lambda_soup_utilities.command
    ; "imdb", Imdb.command
    ; "maze", Maze.command
    ; "file-fetcher-demo", File_fetcher_demo.command
    ]
;;
