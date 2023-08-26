module RPC := Jsonrpc

(** An abstract type representing all of the various resources required to run the LSP server. *)
type io

(** Initialize the abstract io type. *)
val init : Eio_unix.Stdenv.base -> io

(** Receive a JSON RPC packet on stdin. *)
val recv : io -> RPC.Packet.t option

(** Send a JSON RPC packet on stdout. *)
val send : io -> RPC.Packet.t -> unit
