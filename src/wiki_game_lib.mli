open! Core
module File_fetcher = File_fetcher
module Lambda_soup_utilities = Lambda_soup_utilities
module Wiki_game = Wiki_game
module Imdb = Imdb

(** Wrapper around all commands in this library. *)
val command : Command.t
