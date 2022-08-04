open Bwd

type message = DiagnosticSigs.message
module type S = DiagnosticSigs.S

module Make (C : Code.S) : S with module Code := C =
struct
  type nonrec message = message

  type t = {
    code : C.t;
    severity : Severity.t;
    message : message Span.located;
    additional_marks : Span.t list;
    traces : message Span.located bwd;
  }
end
