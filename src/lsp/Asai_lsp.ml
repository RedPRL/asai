open Lsp.Types


module RPC = Jsonrpc

module Make (ErrorCode : Asai.ErrorCode.S) =
struct

  module Effects = Effects.Make(ErrorCode)
  module Doctor = Effects.Doctor
  open Effects

  let unwrap opt err =
    match opt with
    | Some opt -> opt
    | None -> raise @@ LspError err

  let print_exn exn =
    let msg = Printexc.to_string exn
    and stack = Printexc.get_backtrace () in
    Eio.traceln "%s\n%s" msg stack

  (* [TODO: Reed M, 09/06/2022] Commands??? *)
  let supported_code_actions = []
  let supported_commands = []

  let server_capabilities =
    let textDocumentSync =
      let opts = TextDocumentSyncOptions.create
          ~change:(TextDocumentSyncKind.None)
          ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
          ()
      in
      `TextDocumentSyncOptions opts
    in
    let hoverProvider =
      let opts = HoverOptions.create ()
      in `HoverOptions opts
    in
    let codeActionProvider =
      let opts = CodeActionOptions.create ~codeActionKinds:supported_code_actions () in
      `CodeActionOptions opts
    in
    let executeCommandProvider =
      ExecuteCommandOptions.create ~commands:supported_commands ()
    in
    (* [FIXME: Reed M, 09/06/2022] The current verison of the LSP library doesn't support 'positionEncoding' *)
    ServerCapabilities.create
      ~textDocumentSync
      ~hoverProvider
      ~codeActionProvider
      ~executeCommandProvider
      ()

  let get_root (init_params : InitializeParams.t) =
    match init_params.rootUri with
    | Some uri -> Some (DocumentUri.to_path uri)
    | None -> Option.join init_params.rootPath

  module R = Lsp.Client_request

  (** Perform the LSP initialization handshake.
      https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize *)
  let initialize () = 
    let (id, req) =
      unwrap (Request.recv ()) @@
      HandshakeError "Initialization must begin with a request."
    in
    match req with
    | E (Initialize init_params as init_req) ->
      begin
        Eio.traceln "Initializing...";
        let resp = InitializeResult.create ~capabilities:server_capabilities () in
        Request.respond id init_req resp;
        Eio.traceln "Responded!";
        let notif =
          unwrap (Notification.recv ()) @@
          HandshakeError "Initialization must complete with an initialized notification."
        in
        match notif with
        | Initialized ->
          let root = get_root init_params in
          Eio.traceln "Root: %s" (Option.value root ~default:"<no-root>");
          set_root root;
          Eio.traceln "Initialized!"
        | _ ->
          raise @@ LspError (HandshakeError "Initialization must complete with an initialized notification.")
      end
    | (E _) ->
      raise @@ LspError (HandshakeError "Initialization must begin with an initialize request.")

  (** Perform the LSP shutdown sequence.
      See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit *)
  let shutdown () =
    let notif =
      unwrap (Notification.recv ()) @@
      ShutdownError "No requests can be recieved after a shutdown request."
    in match notif with
    | Exit ->
      ()
    | _ ->
      raise @@ LspError (ShutdownError "The only notification that can be recieved after a shutdown request is exit.")

  (** {1 Main Event Loop} *)

  let rec event_loop () =
    match recv () with
    | Some packet ->
      let _ =
        match packet with
        | RPC.Packet.Request req ->
          let resp = Request.handle req in
          send (RPC.Packet.Response resp)
        | RPC.Packet.Notification notif ->
          Notification.handle notif
        | _ ->
          Eio.traceln "Recieved unexpected packet type."
        | exception exn ->
          print_exn exn
      in
      if should_shutdown () then
        shutdown ()
      else
        event_loop ()
    | None ->
      Eio.traceln "Recieved an invalid message. Shutting down...@."

  let run ~init ~load_file =
    Eio_main.run @@ fun env ->
    Effects.run env ~init ~load_file @@ fun () ->
    begin
      initialize ();
      event_loop ()
    end
end
