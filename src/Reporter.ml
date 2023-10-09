open Bwd
open Bwd.Infix

include ReporterSigs

module Make (Code : Code) : S with module Code := Code =
struct

  (* Backtraces *)

  module Traces = Algaeff.Reader.Make (struct type nonrec env = Span.t option * Diagnostic.backtrace end)

  let get_loc() = fst @@ Traces.read()

  let with_loc loc = Traces.scope @@ fun (_, bt) -> loc, bt

  let merge_loc loc f =
    match loc with
    | None -> f()
    | loc -> with_loc loc f

  let get_backtrace() = snd @@ Traces.read()

  let with_backtrace bt = Traces.scope @@ fun (loc, _) -> loc, bt

  let trace_text ?loc text = Traces.scope @@ fun (l, bt) ->
    let loc = match loc with None -> l | Some _ -> loc in
    loc, bt <: {loc; value = text}

  let trace_message (msg : Diagnostic.message) =
    trace_text ?loc:msg.loc msg.value

  let trace ?loc str = trace_text ?loc @@ Diagnostic.text str

  let tracef ?loc = Diagnostic.kmessagef trace_message ?loc

  (* Building messages *)

  let get_severity code = function None -> Code.default_severity code | Some severity -> severity
  let get_merged_loc = function None -> get_loc() | loc -> loc

  let diagnostic ?severity ?loc ?(backtrace=get_backtrace()) ?additional_messages code str =
    Diagnostic.make ?loc:(get_merged_loc loc) ~backtrace ?additional_messages (get_severity code severity) code str

  let kdiagnosticf ?severity ?loc ?(backtrace=get_backtrace()) ?additional_messages k code =
    Diagnostic.kmakef ?loc:(get_merged_loc loc) ~backtrace ?additional_messages k (get_severity code severity) code

  let diagnosticf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf Fun.id ?severity ?loc ?backtrace ?additional_messages code

  (* Emitting messages *)

  type _ Effect.t += Emit : Code.t Diagnostic.t -> unit Effect.t
  exception Fatal of Code.t Diagnostic.t

  let emit_diagnostic d = Effect.perform @@ Emit d
  let fatal_diagnostic d = raise @@ Fatal d

  let handler ~(emit : _ -> unit) ~fatal : _ Effect.Deep.handler =
    { retc = Fun.id;
      exnc = (function Fatal d -> fatal d | exn -> raise exn);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Emit d -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
          Algaeff.Fun.Deep.finally k @@ fun () -> emit d
        | _ -> None }

  (* Runners *)

  let run ?init_loc ?(init_backtrace=Emp) ~emit ~fatal f =
    Traces.run ~env:(init_loc, init_backtrace) @@ fun () ->
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  let try_with ?(emit=emit_diagnostic) ?(fatal=fatal_diagnostic) f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  let map_diagnostic m f =
    try_with
      ~emit:(fun d -> emit_diagnostic (m d))
      ~fatal:(fun d -> fatal_diagnostic (m d))
      f

  let map_text m f = map_diagnostic (Diagnostic.map_text m) f

  (* Convenience functions *)

  let emit ?severity ?loc ?backtrace ?additional_messages code str =
    emit_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?additional_messages code str

  let emitf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf emit_diagnostic ?severity ?loc ?backtrace ?additional_messages code

  let fatal ?severity ?loc ?backtrace ?additional_messages code str =
    fatal_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?additional_messages code str

  let fatalf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf fatal_diagnostic ?severity ?loc ?backtrace ?additional_messages code

  let adopt m (run : ?init_loc:_ -> ?init_backtrace:_ -> emit:_ -> fatal:_ -> _) f =
    run f
      ?init_loc:(get_loc())
      ~init_backtrace:(get_backtrace())
      ~emit:(fun d -> emit_diagnostic (m d))
      ~fatal:(fun d -> fatal_diagnostic (m d))

  let register_printer f =
    Traces.register_printer (fun `Read -> f `Trace);
    Printexc.register_printer @@ function
    | Effect.Unhandled (Emit diag) -> f (`Emit diag)
    | Fatal diag -> f (`Fatal diag)
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled asai effect/exception; use Reporter.run"
end
