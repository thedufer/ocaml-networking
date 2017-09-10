open Core
open Async

val connect
  :  Node.Id.t
  -> (Message.t Pipe.Reader.t * Message.t Pipe.Writer.t) Deferred.Or_error.t
