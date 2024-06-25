open! Core

(** Commands for interacting with IMDB data. *)

val command : Command.t
val get_credits : string -> string list
