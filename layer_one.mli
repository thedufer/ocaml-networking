open Core
open Async
open Sdn_local_protocol

(* Layer-one protocol for getting messages across a single connection:

   The protocol looks like:

   - frame header (4 bytes)
   - length (2 bytes)
   - payload (<length> bytes - between 0 and 65535)
   - CRC (4 bytes)
*)

val reader : Message.t Pipe.Reader.t -> Message.t Pipe.Reader.t

val writer : Message.t Pipe.Writer.t -> Message.t Pipe.Writer.t
