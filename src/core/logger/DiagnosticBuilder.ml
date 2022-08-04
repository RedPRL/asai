open Bwd
open BwdNotation

module type S = DiagnosticBuilderSigs.S

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D =
struct
  type env = Diagnostic.message Span.located bwd
  module Traces = Algaeff.Reader.Make (struct type nonrec env = env end)

  let kmessagef k ?loc ?(additional_marks=[]) ?severity ~code =
    Format.kdprintf @@ fun message -> k @@
    D.{
      code;
      severity = Option.value ~default:(C.default_severity code) severity;
      message = {loc; value = message};
      additional_marks;
      traces = Traces.read ();
    }

  let messagef ?loc ?additional_marks ?severity ~code =
    kmessagef Fun.id ?loc ?additional_marks ?severity ~code

  let append_marks d marks =
    D.{ d with additional_marks = d.additional_marks @ marks }

  let tracef ?loc fmt =
    fmt |> Format.kdprintf @@ fun message f ->
    Traces.scope (fun bt -> bt #< { loc; value = message }) f

  let run f = Traces.run ~env:Emp f

end
