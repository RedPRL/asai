open Lsp.Types

module RPC := Jsonrpc

module Make (ErrorCode : Asai.ErrorCode.S) : sig
  type packed = Lsp.Client_request.packed
  type 'resp t = 'resp Lsp.Client_request.t
  type msg = RPC.Id.t option RPC.Message.t

  val handle : RPC.Id.t -> msg -> RPC.Response.t
  val recv : unit -> (RPC.Id.t * packed) option
  val respond : RPC.Id.t -> 'resp t -> 'resp -> unit
end
