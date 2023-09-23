open Bwd
open Bwd.Infix

module type S = LoggerSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code =
struct

  (* Backtraces *)

  module Traces = Algaeff.Reader.Make (struct type nonrec env = Diagnostic.backtrace end)

  let get_backtrace = Traces.read

  let retrace bt = Traces.run ~env:bt

  let trace fr f = Traces.scope (fun bt -> bt <: fr) f

  let trace_string ?loc str f = trace (Diagnostic.message_of_string ?loc str) f

  let tracef ?loc = Diagnostic.kmessagef trace ?loc

  (* Building messages *)

  let diagnostic_of_message ?severity ?(backtrace=get_backtrace()) ?(additional_messages=[]) code message =
    Diagnostic.{
      severity = Option.value ~default:(Code.default_severity code) severity;
      code;
      message;
      backtrace;
      additional_messages;
    }

  let kdiagnosticf k ?severity ?loc ?backtrace ?additional_messages code =
    Diagnostic.kmessagef ?loc (fun msg -> k (diagnostic_of_message ?severity ?backtrace ?additional_messages code msg))

  let diagnostic_of_string ?severity ?loc ?backtrace ?additional_messages code str =
    diagnostic_of_message ?severity ?backtrace ?additional_messages code @@
    Diagnostic.message_of_string ?loc str

  let diagnosticf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf Fun.id ?severity ?loc ?backtrace ?additional_messages code

  (* Emitting messages *)

  type _ Effect.t += Emit : Code.t Diagnostic.t -> unit Effect.t
  exception Fatal of Code.t Diagnostic.t

  let emit d = Effect.perform @@ Emit d
  let fatal d = raise @@ Fatal d

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

  let try_with ?(emit=emit) ?(fatal=fatal) f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  (* Convenience functions *)

  let emit_string ?severity ?loc ?backtrace ?additional_messages code str =
    emit @@ diagnostic_of_string ?severity ?loc ?backtrace ?additional_messages code str

  let emitf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf emit ?severity ?loc ?backtrace ?additional_messages code

  let fatal_string ?severity ?loc ?backtrace ?additional_messages code str =
    fatal @@ diagnostic_of_string ?severity ?loc ?backtrace ?additional_messages code str

  let fatalf ?severity ?loc ?backtrace ?additional_messages code =
    kdiagnosticf fatal ?severity ?loc ?backtrace ?additional_messages code

  let adopt m (run : ?init_backtrace:_ -> emit:_ -> fatal:_ -> _) f =
    run f
      ~init_backtrace:(get_backtrace())
      ~emit:(fun d -> emit (m d))
      ~fatal:(fun d -> fatal (m d))

  let register_printer f =
    Printexc.register_printer @@ function
    | Effect.Unhandled (Emit diag) -> f (`Emit diag)
    | Fatal diag -> f (`Fatal diag)
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled asai effect/exception; use Asai.Logger.run"
end
