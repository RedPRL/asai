module L := Lsp.Types
module RPC := Jsonrpc

module Make (Message : Asai.MinimumSigs.Message) : sig
  type lsp_error =
    | DecodeError of string
    | HandshakeError of string
    | ShutdownError of string
    | UnknownRequest of string
    | UnknownNotification of string

  exception LspError of lsp_error

  val recv : unit -> RPC.Packet.t option
  val send : RPC.Packet.t -> unit

  val should_shutdown : unit -> bool
  val initiate_shutdown : unit -> unit

  val set_root : string option -> unit
  val load_file : L.DocumentUri.t -> unit

  module Request : sig
    type packed = Lsp.Client_request.packed
    type 'resp t = 'resp Lsp.Client_request.t

    val handle : RPC.Request.t -> RPC.Response.t
    val recv : unit -> (RPC.Id.t * packed) option
    val respond : RPC.Id.t -> 'resp t -> 'resp -> unit
  end

  module Notification : sig
    type t = Lsp.Client_notification.t

    val handle : RPC.Notification.t -> unit
    val recv : unit -> t option
  end

  val run : Eio_unix.Stdenv.base
    -> ?source:string
    -> init:(root:string option -> unit)
    -> load_file:(display:(Message.t Asai.Diagnostic.t -> unit) -> string -> unit)
    -> (unit -> 'a)
    -> 'a
end
