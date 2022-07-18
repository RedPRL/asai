open Bwd
open Loc

module StringTbl := Hashtbl.Make(String)

(** The signature of a diagnostic  *)
module type S = sig
  (** An abstract type of error codes. *)
  type code

  (** The type of error messages. 

      When we render a diagnostic, the layout engine of the rendering backend should be the
      one making layout choices. This causes problems when error messages are strings;
      all layout has already been performed, and we have no hope of doing it again
      correctly! Therefore, encode error messages as things awaiting a formatter, which
      allows the rendering backend to handle all layout-related decisions.

      This is best paired with {!Format.dprintf}, which allows us to delay all formatting
      choices.
  *)
  type message = Format.formatter -> unit

  (** A cause is some source span, along with a message
      associated with the span. *)
  type cause = {
    location : Span.t;
    message : message
  }

  (** The type of diagnostic messages. *)
  type t = {
    message : message;
    (** The main message of a diagnostic. *)
    code : code;
    (** The error code of a diagnostic. *)
    cause : cause;
    (** The main cause of an error. *)
    frames : cause bwd;
    (** A stack of extra information that may be relevant to the error. *)
  }

  (** Construct *)
  val build : code:code -> cause:cause -> message -> t

  (** The severity of a diagnostic. *)
  val severity : t -> Severity.t

  type display = buffers:(string StringTbl.t) -> t -> unit
end

(** The functor used to generate diagnostics from an error code. *)
module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t
