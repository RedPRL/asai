open Bwd
open Loc

module StringTbl = Hashtbl.Make(String)

module type S =
sig
  type code

  type cause = {
    location : Span.t;
    message : string
  }

  type t = {
    message : string;
    code : code;
    cause : cause;
    frames : cause bwd
  }

  val build : code:code -> cause:cause -> string -> t

  val severity : t -> Severity.t

  type display = buffers:(string StringTbl.t) -> t -> unit
end

module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t =
struct
  type cause = {
    location : Span.t;
    message : string
  }

  type t = {
    message : string;
    code : ErrorCode.t;
    cause : cause;
    frames : cause bwd
  }

  let severity diag =
    ErrorCode.severity diag.code

  type code = ErrorCode.t

  let build ~code ~cause message = {
    message;
    code;
    cause;
    frames = Emp
  }

  type display = buffers:(string StringTbl.t) -> t -> unit
end
