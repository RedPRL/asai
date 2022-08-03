open Bwd

type message = Format.formatter -> unit

module type S =
sig
  module Code : Code.S

  type nonrec message = message

  type t = {
    code : Code.t;
    message : message Span.located;
    additional_marks : Span.t list;
    traces : message Span.located bwd;
  }

  val code_id : t -> Severity.t * int
end

module Make (C : Code.S) : S with module Code := C =
struct
  type nonrec message = message

  type t = {
    code : C.t;
    message : message Span.located;
    additional_marks : Span.t list;
    traces : message Span.located bwd;
  }

  let code_id d = C.id d.code
end
