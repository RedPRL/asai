open Bwd
open Loc

(** The signature of a diagnostic  *)
module type S = sig
  (** An abstract type of error codes. *)
  type code

  (** A highlight is some source span, along with a message
      associated with the span. *)
  type highlight = {
    location : Span.t;
    message : string
  }

  (** The "cause" of a diagnostic.
      These are relevant spans that may be associated
      with some diagnostic, along with an optional
      highlight. *)
  type cause = {
    location : Span.t;
    highlight : highlight option
  }

  (** The type of diagnostic messages. *)
  type t = {
    message : string;
    (** The main message of a diagnostic. *)
    code : code;
    (** The error code of a diagnostic. *)
    causes : cause bwd;
    (** A stack of "causes" of a diagnostic. *)
  }

  (** Construct *)
  val build : code:code -> string -> t
  val with_cause : location:Span.t -> t -> t
  val with_highlight : location:Span.t -> message:string -> t -> t

  (** The severity of a diagnostic. *)
  val severity : t -> Severity.t
end

(** The functor used to generate diagnostics from an error code. *)
module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t
