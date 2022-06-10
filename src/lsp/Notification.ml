open Lsp.Types

module Make (ErrorCode : Asai.ErrorCode.S) =
struct
  open Effects.Make(ErrorCode)

  module RPC = Jsonrpc
  module Notification = Lsp.Client_notification

  type msg = RPC.Id.t option RPC.Message.t
  type t = Lsp.Client_notification.t

  let dispatch : string -> t -> unit = 
    fun mthd ->
    function
    | TextDocumentDidOpen doc ->
      load_file doc.textDocument.uri
    | DidSaveTextDocument doc ->
      load_file doc.textDocument.uri
    | _ ->
      raise @@ LspError (UnknownNotification mthd)

  let handle (msg : msg) =
    Eio.traceln "Request: %s@." msg.method_;
    match Notification.of_jsonrpc { msg with id = () } with
    | Ok notif ->
      dispatch msg.method_ notif
    | Error err ->
      raise @@ LspError (DecodeError err)

  let recv () =
    Option.bind (recv ()) @@
    function
    | Jsonrpc.Message ({ id = None; _ } as msg) ->
      begin
        match Notification.of_jsonrpc { msg with id = () } with
        | Ok notif -> Some notif
        | Error err -> raise @@ LspError (DecodeError err)
      end
    | _ -> None
end
