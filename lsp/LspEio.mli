open Eio

(** An abstract type representing all of the various resources required to run the LSP server. *)
type io

(** Initialize the abstract io type. *)
val init : Stdenv.t -> io

(** Receive a JSON RPC packet on stdin. *)
val recv : io -> Jsonrpc.packet option

(** Send aa JSON RPC packet on stdout. *)
val send : io -> Jsonrpc.packet -> unit
