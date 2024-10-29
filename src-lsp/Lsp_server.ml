open Bwd

module L = Lsp.Types
module RPC = Jsonrpc
module Broadcast = Lsp.Server_notification
module Lsp_Diagnostic = Lsp.Types.Diagnostic
module Request = Lsp.Client_request
module Notification = Lsp.Client_notification

module Make (Message : Asai.Minimum_signatures.Message) =
struct
  type diagnostic = Message.t Asai.Diagnostic.t

  type server = {
    lsp_io : Lsp_eio.io;
    should_shutdown : bool;
    source : string option;
    init:root : string option -> unit;
    load_file : display:(Message.t Asai.Diagnostic.t -> unit) -> string -> unit;
  }

  module State = Algaeff.State.Make(struct type t = server end)

  type lsp_error =
    | Decode_error of string
    | Handshake_error of string
    | Shutdown_error of string
    | Unknown_request of string
    | Unknown_notification of string

  exception Lsp_error of lsp_error

  let () = Printexc.register_printer @@
    function
    | Lsp_error (Decode_error err) ->
      Some (Format.asprintf "Lsp Error: Couldn't decode %s" err)
    | Lsp_error (Handshake_error err) ->
      Some (Format.asprintf "Lsp Error: Invalid initialization handshake %s" err)
    | Lsp_error (Shutdown_error err) ->
      Some (Format.asprintf "Lsp Error: Invalid shutdown sequence %s" err)
    | Lsp_error (Unknown_request err) ->
      Some (Format.asprintf "Lsp Error: Unknown request %s" err)
    | Lsp_error (Unknown_notification err) ->
      Some (Format.asprintf "Lsp Error: Unknown notification %s" err)
    | _ -> None

  let recv () =
    let server = State.get () in
    Lsp_eio.recv server.lsp_io

  let send packet =
    let server = State.get () in
    Lsp_eio.send server.lsp_io packet

  let broadcast notif =
    let msg = Broadcast.to_jsonrpc notif in
    send (RPC.Packet.Notification msg)

  let render_lsp_related_info (uri : L.DocumentUri.t) (message : Asai.Loctext.t) : L.DiagnosticRelatedInformation.t =
    let range = Lsp_shims.Loc.lsp_range_of_range message.loc in
    let location = L.Location.create ~uri ~range in
    let message = Asai.Text.to_string message.value in
    L.DiagnosticRelatedInformation.create ~location ~message

  let render_lsp_diagnostic (uri : L.DocumentUri.t) (diag : diagnostic) : Lsp_Diagnostic.t =
    let range = Lsp_shims.Loc.lsp_range_of_range diag.explanation.loc in
    let severity = Lsp_shims.Diagnostic.lsp_severity_of_severity @@ diag.severity in
    let code = `String (Message.short_code diag.message) in
    let source = (State.get ()).source in
    let message = Asai.Text.to_string diag.explanation.value in
    let relatedInformation = Bwd.to_list @@ Bwd.map (render_lsp_related_info uri) diag.extra_remarks in
    Lsp_Diagnostic.create
      ~range
      ~severity
      ~code
      ?source
      ~message:(`String message)
      ~relatedInformation
      ()

  let publish_diagnostics path (diagnostics : diagnostic list) =
    let uri = L.DocumentUri.of_path path in
    let diagnostics = List.map (render_lsp_diagnostic uri) diagnostics in
    let params = L.PublishDiagnosticsParams.create ~uri ~diagnostics () in
    broadcast (PublishDiagnostics params)

  let set_root root =
    let server = State.get () in
    server.init ~root

  let load_file uri =
    let server = State.get () in
    let path = L.DocumentUri.to_path uri in
    Eio.traceln "Loading file: %s@." path;
    (* The LSP protocol doesn't allow for incremental publishing of diagnostics.
       Therefore, we need to accumulate all the diagnostics encountered during
       a run, and publish them in one go. *)
    let diagnostics = ref [] in
    let push_diagnostic d =
      diagnostics := d :: !diagnostics
    in
    server.load_file ~display:push_diagnostic path;
    publish_diagnostics path !diagnostics

  let should_shutdown () =
    let server = State.get () in
    server.should_shutdown

  let initiate_shutdown () =
    State.modify @@ fun st -> { st with should_shutdown = true }

  (* [TODO: Reed M, 12/12/2022] No code actions for now. *)
  let code_action (_params : L.CodeActionParams.t) : L.CodeActionResult.t =
    None

  (* [TODO: Reed M, 12/12/2022] No hovers for now. *)
  let hover (_params : L.HoverParams.t) : L.Hover.t option =
    None

  module Request =
  struct
    type 'resp t = 'resp Lsp.Client_request.t
    type packed = Request.packed

    let dispatch : type resp. string -> resp t -> resp =
      fun mthd ->
      function
      | Initialize _ ->
        let err = "Server can only recieve a single initialization request." in
        raise @@ Lsp_error (Handshake_error err)
      | Shutdown ->
        initiate_shutdown ()
      | CodeAction params ->
        code_action params
      | TextDocumentHover params ->
        hover params
      | _ ->
        raise @@ Lsp_error (Unknown_request mthd)

    let handle (msg : RPC.Request.t) =
      Eio.traceln "Request: %s@." msg.method_;
      match Request.of_jsonrpc msg with
      | Ok (E r) ->
        let resp = dispatch msg.method_ r in
        let json = Request.yojson_of_result r resp in
        RPC.Response.ok msg.id json
      | Error err ->
        raise (Lsp_error (Decode_error err))

    let recv () =
      Option.bind (recv ()) @@
      function
      | RPC.Packet.Request req ->
        begin
          match Request.of_jsonrpc req with
          | Ok packed -> Some (req.id, packed)
          | Error err -> raise @@ Lsp_error (Decode_error err)
        end
      | _ -> None

    let respond id req resp =
      let json = Request.yojson_of_result req resp in
      send (RPC.Packet.Response (RPC.Response.ok id json))
  end

  module Notification =
  struct
    type t = Lsp.Client_notification.t

    let dispatch : string -> t -> unit =
      fun mthd ->
      function
      | TextDocumentDidOpen doc ->
        load_file doc.textDocument.uri
      | DidSaveTextDocument doc ->
        load_file doc.textDocument.uri
      | _ ->
        raise @@ Lsp_error (Unknown_notification mthd)

    let handle (msg : RPC.Notification.t) =
      Eio.traceln "Request: %s@." msg.method_;
      match Notification.of_jsonrpc msg with
      | Ok notif ->
        dispatch msg.method_ notif
      | Error err ->
        raise @@ Lsp_error (Decode_error err)

    let recv () =
      Option.bind (recv ()) @@
      function
      | RPC.Packet.Notification msg ->
        begin
          match Notification.of_jsonrpc msg with
          | Ok notif -> Some notif
          | Error err -> raise @@ Lsp_error (Decode_error err)
        end
      | _ -> None
  end

  let run env ?source ~init ~load_file k =
    let lsp_io = Lsp_eio.init env in
    let init = {
      lsp_io;
      source;
      init;
      load_file;
      should_shutdown = false;
    }
    in State.run ~init k
end
