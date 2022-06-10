open Lsp.Types

module RPC = Jsonrpc
module Broadcast = Lsp.Server_notification

type server = {
  lsp_io : LspEio.io;
  should_shutdown : bool;
  root : string option
}

type _ Effect.t +=
  | LoadFile : string -> unit Effect.t

module State = Algaeff.State.Make(struct type state = server end)

let recv () = 
  let server = State.get () in
  LspEio.recv server.lsp_io

let send packet =
  let server = State.get () in
  LspEio.send server.lsp_io packet

let broadcast notif =
  let msg = Broadcast.to_jsonrpc notif in
  send (RPC.Message { msg with id = None })

let publish_diagnostics path diagnostics =
  let uri = DocumentUri.of_path path in
  let params = PublishDiagnosticsParams.create ~uri ~diagnostics () in
  broadcast (PublishDiagnostics params)

let set_root root =
  State.modify @@ fun st -> { st with root }

let load_file uri =
  let path = DocumentUri.to_path uri in
  Eio.traceln "Loading file: %s@." path;
  Effect.perform (LoadFile path);
  (* [TODO: Reed M, 09/06/2022] Actually publish the diagnostics *)
  publish_diagnostics path []

let should_shutdown () =
  let server = State.get () in
  server.should_shutdown

let initiate_shutdown () =
  State.modify @@ fun st -> { st with should_shutdown = true }

let run env k = 
  let lsp_io = LspEio.init env in
  let init = {
    lsp_io;
    should_shutdown = false;
    root = None
  }
  in State.run ~init k

type lsp_error =
  | DecodeError of string
  | HandshakeError of string
  | ShutdownError of string
  | UnknownRequest of string
  | UnknownNotification of string

exception LspError of lsp_error

let () = Printexc.register_printer @@
  function
  | LspError (DecodeError err) -> Some (Format.asprintf "Lsp Error: Couldn't decode %s" err)
  | LspError (HandshakeError err) -> Some (Format.asprintf "Lsp Error: Invalid initialization handshake %s" err)
  | LspError (ShutdownError err) -> Some (Format.asprintf "Lsp Error: Invalid shutdown sequence %s" err)
  | LspError (UnknownRequest err) -> Some (Format.asprintf "Lsp Error: Unknown request %s" err)
  | LspError (UnknownNotification err) -> Some (Format.asprintf "Lsp Error: Unknown notification %s" err)
  | _ -> None
