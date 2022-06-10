open Lsp.Types

module RPC := Jsonrpc

type msg = RPC.Id.t option RPC.Message.t
type t = Lsp.Client_notification.t

val handle : msg -> unit
val recv : unit -> t option
