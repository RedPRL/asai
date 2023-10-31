open Bwd
open Bwd.Infix

include ReporterSigs

module Make (Message : Message) : S with module Message := Message =
struct

  (* Backtraces *)

  module Traces = Algaeff.Reader.Make (struct type t = Range.t option * Diagnostic.backtrace end)

  let get_loc() = fst @@ Traces.read()

  let with_loc loc = Traces.scope @@ fun (_, bt) -> loc, bt

  let merge_loc loc f =
    match loc with
    | None -> f()
    | loc -> with_loc loc f

  let get_backtrace() = snd @@ Traces.read()

  let with_backtrace bt = Traces.scope @@ fun (loc, _) -> loc, bt

  let trace_text ?loc text = Traces.scope @@ fun (l, bt) ->
    (match loc with None -> l | Some _ -> loc),
    bt <: {loc; value = text}

  let trace_loctext (t : Diagnostic.loctext) =
    trace_text ?loc:t.loc t.value

  let trace ?loc str = trace_text ?loc @@ Diagnostic.text str

  let tracef ?loc = Diagnostic.kloctextf trace_loctext ?loc

  (* Constructing diagnostics *)

  let get_severity message = function None -> Message.default_severity message | Some severity -> severity
  let get_merged_loc = function None -> get_loc() | loc -> loc

  let diagnostic ?severity ?loc ?(backtrace=get_backtrace()) ?extra_remarks message explanation =
    Diagnostic.make ?loc:(get_merged_loc loc) ~backtrace ?extra_remarks (get_severity message severity) message explanation

  let kdiagnosticf ?severity ?loc ?(backtrace=get_backtrace()) ?extra_remarks k message =
    Diagnostic.kmakef ?loc:(get_merged_loc loc) ~backtrace ?extra_remarks k (get_severity message severity) message

  let diagnosticf ?severity ?loc ?backtrace ?extra_remarks message =
    kdiagnosticf Fun.id ?severity ?loc ?backtrace ?extra_remarks message

  (* Sending diagnostics *)

  type _ Effect.t += Emit : Message.t Diagnostic.t -> unit Effect.t
  exception Fatal of Message.t Diagnostic.t

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

  (* Algebraic effects *)

  let run ?init_loc ?(init_backtrace=Emp) ~emit ~fatal f =
    Traces.run ~env:(init_loc, init_backtrace) @@ fun () ->
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  let try_with ?(emit=emit_diagnostic) ?(fatal=fatal_diagnostic) f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  (* Convenience functions *)

  let emit ?severity ?loc ?backtrace ?extra_remarks message explanation =
    emit_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?extra_remarks message explanation

  let emitf ?severity ?loc ?backtrace ?extra_remarks message =
    kdiagnosticf emit_diagnostic ?severity ?loc ?backtrace ?extra_remarks message

  let fatal ?severity ?loc ?backtrace ?extra_remarks message explanation =
    fatal_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?extra_remarks message explanation

  let fatalf ?severity ?loc ?backtrace ?extra_remarks message =
    kdiagnosticf fatal_diagnostic ?severity ?loc ?backtrace ?extra_remarks message

  let adopt m (run : ?init_loc:_ -> ?init_backtrace:_ -> emit:_ -> fatal:_ -> _) f =
    run f
      ?init_loc:(get_loc())
      ~init_backtrace:(get_backtrace())
      ~emit:(fun d -> emit_diagnostic (m d))
      ~fatal:(fun d -> fatal_diagnostic (m d))

  let map_diagnostic m f =
    try_with
      ~emit:(fun d -> emit_diagnostic (m d))
      ~fatal:(fun d -> fatal_diagnostic (m d))
      f

  (* Debugging *)

  let register_printer f =
    Traces.register_printer (fun `Read -> f `Trace);
    Printexc.register_printer @@ function
    | Effect.Unhandled (Emit diag) -> f (`Emit diag)
    | Fatal diag -> f (`Fatal diag)
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled asai effect/exception; use Reporter.run"
end
