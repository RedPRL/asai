open Bwd
open Loc

module StringTbl = Hashtbl.Make(String)

let foo = Format.asprintf "%s" "foo"

module type S =
sig
  type code
  type message = Format.formatter -> unit

  type cause = {
    location : Span.t;
    message : message
  }

  type t = {
    message : message;
    code : code;
    cause : cause;
    frames : cause bwd
  }

  val build : code:code -> cause:cause -> message -> t

  val severity : t -> Severity.t

  type display = buffers:(string StringTbl.t) -> t -> unit
end

module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t =
struct
  type code = ErrorCode.t
  type message = Format.formatter -> unit

  type cause = {
    location : Span.t;
    message : message
  }

  type t = {
    message : message;
    code : ErrorCode.t;
    cause : cause;
    frames : cause bwd
  }

  let severity diag =
    ErrorCode.severity diag.code


  let build ~code ~cause message = {
    message;
    code;
    cause;
    frames = Emp
  }

  type display = buffers:(string StringTbl.t) -> t -> unit
end
