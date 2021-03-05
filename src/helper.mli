open! Core
open Async
open Sdn_local_protocol_kernel

val connect
  :  Node.Id.t
  -> (Message.t Pipe.Reader.t * Message.t Pipe.Writer.t * Address.t * int) Deferred.Or_error.t
