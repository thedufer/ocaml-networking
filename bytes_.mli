open! Core

type t = char list

val of_int : int -> num_bytes:int -> char list
val to_int : char list -> num_bytes:int -> int
