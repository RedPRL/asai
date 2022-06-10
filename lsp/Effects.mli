open Lsp.Types
module RPC := Jsonrpc

type _ Effect.t +=
  | LoadFile : string -> unit Effect.t

val recv : unit -> RPC.packet option
val send : RPC.packet -> unit

val should_shutdown : unit -> bool
val initiate_shutdown : unit -> unit

val set_root : string option -> unit
val load_file : DocumentUri.t -> unit

type lsp_error =
  | DecodeError of string
  | HandshakeError of string
  | ShutdownError of string
  | UnknownRequest of string
  | UnknownNotification of string

exception LspError of lsp_error

val run : Eio.Stdenv.t ->  (unit -> 'a) -> 'a
