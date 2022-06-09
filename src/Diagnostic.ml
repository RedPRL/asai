open Bwd
open Loc

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
    causes : cause bwd
  }

  val build : code:code -> string -> t
  val with_cause : location:Span.t -> message:string -> t -> t

  val severity : t -> Severity.t
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
    causes : cause bwd
  }

  let severity diag =
    ErrorCode.severity diag.code

  type code = ErrorCode.t

  let build ~code message = {
    message;
    code;
    causes = Emp
  }

  let with_cause ~location ~message diag =
    let cause = { location; message } in
    { diag with causes = Snoc(diag.causes, cause) }
end
