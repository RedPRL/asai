open Lsp.Types
open Effects

module RPC = Jsonrpc
module Request = Lsp.Client_request

type msg = RPC.Id.t option RPC.Message.t
type 'resp t = 'resp Lsp.Client_request.t
type packed = Request.packed

let dispatch : type resp. string -> resp t -> resp =
  fun mthd ->
  function
  | Initialize _ ->
    let err = "Server can only recieve a single initialization request." in
    raise @@ LspError (HandshakeError err)
  | Shutdown ->
    initiate_shutdown ()
  | _ ->
    raise @@ LspError (UnknownRequest mthd)

let handle id (msg : msg) =
  Eio.traceln "Request: %s@." msg.method_;
  match Request.of_jsonrpc { msg with id } with
  | Ok (E r) ->
    let resp = dispatch msg.method_ r in
    let json = Request.yojson_of_result r resp in
    RPC.Response.ok id json
  | Error err ->
    raise (LspError (DecodeError err))

let recv () =
  Option.bind (recv ()) @@
  function
  | Jsonrpc.Message ({ id = Some id; _ } as msg) ->
    begin
      match Request.of_jsonrpc { msg with id } with
      | Ok packed -> Some (id, packed)
      | Error err -> raise @@ LspError (DecodeError err)
    end
  | _ -> None

let respond id req resp =
  let json = Request.yojson_of_result req resp in
  send (RPC.Response (RPC.Response.ok id json))
