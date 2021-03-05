open! Core
open Async

type 'a t

val create : size:int -> 'a t

val write : 'a t -> 'a -> unit Deferred.t
val write_without_pushback : 'a t -> 'a -> unit

val get : 'a t -> 'a list * 'a Pipe.Reader.t
