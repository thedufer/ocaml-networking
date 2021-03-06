open! Core
open Async
open Sdn_local_protocol

(* Layer-two protocol for getting messages across a local network:

   The protocol looks like:

   - destination address (8 bytes)
   - source address (8 bytes)
   - payload (0-65519 bytes)

   Then, this is passed as the payload in a layer-one frame.
*)

type t = {
  msg : Message.t;
  to_ : Address.t;
  from : Address.t;
}

val reader : Message.t Pipe.Reader.t -> t Pipe.Reader.t

val writer : Message.t Pipe.Writer.t -> t Pipe.Writer.t

val hub
  :  Message.t Pipe.Reader.t
  -> Message.t Pipe.Writer.t
  -> ports:int
  -> unit Deferred.t

val switch
  :  Message.t Pipe.Reader.t
  -> Message.t Pipe.Writer.t
  -> ports:int
  -> expiration:Time.Span.t
  -> unit Deferred.t

val stp_switch
  :  Message.t Pipe.Reader.t
  -> Message.t Pipe.Writer.t
  -> ports:int
  -> me:Address.Node.t
  -> unit Deferred.t
