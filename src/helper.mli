open! Core
open Async

val connect
  :  Node.Id.t
  -> (Message.t Pipe.Reader.t * Message.t Pipe.Writer.t * Address.t * int) Deferred.Or_error.t
