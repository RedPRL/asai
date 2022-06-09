open Bwd
open Loc

module type S =
sig
  type code

  type highlight = {
    location : Span.t;
    message : string
  }

  type cause = {
    location : Span.t;
    highlight : highlight option
  }

  type t = {
    message : string;
    code : code;
    causes : cause bwd
  }

  val build : code:code -> string -> t
  val with_cause : location:Span.t -> t -> t
  val with_highlight : location:Span.t -> message:string -> t -> t

  val severity : t -> Severity.t
end

module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t =
struct
  type highlight = {
    location : Span.t;
    message : string
  }

  type cause = {
    location : Span.t;
    highlight : highlight option
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

  let with_cause ~location diag =
    let cause = { location = location; highlight = None } in
    { diag with causes = Snoc(diag.causes, cause) }

  let with_highlight ~location ~message diag =
    match diag.causes with
    | Snoc(causes, cause) ->
      let highlight = Some ({location; message}) in
      { diag with causes = Snoc(causes, { cause with highlight}) }
    | Emp ->
      diag
end
