open Lsp.Types
module RPC := Jsonrpc

type lsp_error =
  | DecodeError of string
  | HandshakeError of string
  | ShutdownError of string
  | UnknownRequest of string
  | UnknownNotification of string

exception LspError of lsp_error

val recv : unit -> RPC.packet option
val send : RPC.packet -> unit

val should_shutdown : unit -> bool
val initiate_shutdown : unit -> unit

val set_root : string option -> unit
val load_file : DocumentUri.t -> unit


val run : Eio.Stdenv.t
  -> init:(string option -> unit)
  -> load_file:(string -> unit)
  -> (unit -> 'a)
  -> 'a
