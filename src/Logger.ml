open Bwd
open Bwd.Infix

module type S = LoggerSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code =
struct

  (* Backtraces *)

  module Traces = Algaeff.Reader.Make (struct type nonrec env = Diagnostic.backtrace end)

  let get_backtrace = Traces.read

  let with_backtrace bt = Traces.run ~env:bt

  let trace_message msg = Traces.scope @@ fun bt -> bt <: msg

  let trace ?loc str = trace_message @@ Diagnostic.message ?loc str

  let tracef ?loc = Diagnostic.kmessagef trace_message ?loc

  (* Building messages *)

  let get_severity code = function None -> Code.default_severity code | Some severity -> severity

  let diagnostic ?severity ?loc ?(backtrace=get_backtrace()) ?additional_messages code str =
    Diagnostic.make ?loc ~backtrace ?additional_messages (get_severity code severity) code str

  let kdiagnosticf ?severity ?loc ?(backtrace=get_backtrace()) ?additional_messages k code =
    Diagnostic.kmakef ?loc ~backtrace ?additional_messages k (get_severity code severity) code

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

  let run ?(init_backtrace=Emp) ~emit ~fatal f =
    Traces.run ~env:init_backtrace @@ fun () ->
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  let try_with ?(emit=emit_diagnostic) ?(fatal=fatal_diagnostic) f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  (* Convenience functions *)

  let emit ?severity ?loc ?backtrace ?additional_messages code str =
    emit_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?additional_messages code str

  let emitf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf emit_diagnostic ?severity ?loc ?backtrace ?additional_messages code

  let fatal ?severity ?loc ?backtrace ?additional_messages code str =
    fatal_diagnostic @@ diagnostic ?severity ?loc ?backtrace ?additional_messages code str

  let fatalf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf fatal_diagnostic ?severity ?loc ?backtrace ?additional_messages code

  let adopt m (run : ?init_backtrace:_ -> emit:_ -> fatal:_ -> _) f =
    run f
      ~init_backtrace:(get_backtrace())
      ~emit:(fun d -> emit_diagnostic (m d))
      ~fatal:(fun d -> fatal_diagnostic (m d))

  let register_printer f =
    Printexc.register_printer @@ function
    | Effect.Unhandled (Emit diag) -> f (`Emit diag)
    | Fatal diag -> f (`Fatal diag)
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled asai effect/exception; use Asai.Logger.run"
end
