open Bwd
open Loc

module StringTbl := Hashtbl.Make(String)

(** The signature of a diagnostic  *)
module type S = sig
  (** An abstract type of error codes. *)
  type code

  (** A cause is some source span, along with a message
      associated with the span. *)
  type cause = {
    location : Span.t;
    message : string
  }

  (** The type of diagnostic messages. *)
  type t = {
    message : string;
    (** The main message of a diagnostic. *)
    code : code;
    (** The error code of a diagnostic. *)
    cause : cause;
    (** The main cause of an error. *)
    frames : cause bwd;
    (** A stack of extra information that may be relevant to the error. *)
  }

  (** Construct *)
  val build : code:code -> cause:cause -> string -> t

  (** The severity of a diagnostic. *)
  val severity : t -> Severity.t

  type display = buffers:(string StringTbl.t) -> t -> unit
end

(** The functor used to generate diagnostics from an error code. *)
module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t
