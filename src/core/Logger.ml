open Bwd
open Bwd.Infix

module type S = LoggerSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code =
struct

  (* Backtraces *)

  type env = Diagnostic.message Span.located bwd
  module Traces = Algaeff.Reader.Make (struct type nonrec env = env end)

  let get_backtrace = Traces.read

  let ktracef k ?loc fmt =
    fmt |> Format.kdprintf @@ fun message x ->
    Traces.scope (fun bt -> bt <: { loc; value = message }) @@ k x

  let tracef ?loc fmt = ktracef Fun.id ?loc fmt

  let retrace bt = Traces.run ~env:bt

  (* Building messages *)

  let kmessagef k ?loc ?(additional_marks=[]) ?severity code =
    Format.kdprintf @@ fun message -> k @@
    Diagnostic.{
      code;
      severity = Option.value ~default:(Code.default_severity code) severity;
      message = {loc; value = message};
      additional_marks;
      backtrace = get_backtrace();
    }

  let messagef ?loc ?additional_marks ?severity code =
    kmessagef Fun.id ?loc ?additional_marks ?severity code

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

  let emitf ?loc ?additional_marks ?severity code = kmessagef emit ?loc ?additional_marks ?severity code
  let fatalf ?loc ?additional_marks ?severity code = kmessagef fatal ?loc ?additional_marks ?severity code

  let adopt m (run : ?init_backtrace:_ -> emit:_ -> fatal:_ -> _) f =
    run f
      ~init_backtrace:(get_backtrace())
      ~emit:(fun d -> emit (m d))
      ~fatal:(fun d -> fatal (m d))
end
