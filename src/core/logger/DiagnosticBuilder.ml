open Bwd
open Bwd.Infix

module type S = DiagnosticBuilderSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  type env = Diagnostic.message Span.located bwd
  module Traces = Algaeff.Reader.Make (struct type nonrec env = env end)

  let backtrace = Traces.read

  let kmessagef k ?loc ?(additional_marks=[]) ?severity code =
    Format.kdprintf @@ fun message -> k @@
    Diagnostic.{
      code;
      severity = Option.value ~default:(Code.default_severity code) severity;
      message = {loc; value = message};
      additional_marks;
      backtrace = Traces.read ();
    }

  let messagef ?loc ?additional_marks ?severity code =
    kmessagef Fun.id ?loc ?additional_marks ?severity code

  let append_marks d marks =
    Diagnostic.{ d with additional_marks = d.additional_marks @ marks }

  let ktracef k ?loc fmt =
    fmt |> Format.kdprintf @@ fun message x ->
    Traces.scope (fun bt -> bt #< { loc; value = message }) @@ k x

  let tracef ?loc fmt = ktracef Fun.id ?loc fmt

  let run ?(init=Emp) f = Traces.run ~env:init f

end
