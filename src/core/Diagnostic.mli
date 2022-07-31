open Bwd

(** The type of single messages. 

    When we render a diagnostic, the layout engine of the rendering backend should be the one making layout choices. Therefore, we cannot pass already formatted strings but a function awaiting a formatter. This is best paired with {!val:Format.dprintf}, which allows us to delay formatting choices. *)
type message = Format.formatter -> unit

(** The signature of a diagnostic module. *)
module type S = sig

  (** The module of error codes. *)
  module Code : Code.S

  type nonrec message = message

  (** The type of diagnostics. *)
  type t = {
    code : Code.t;
    (** The error code. *)
    message : message Span.located;
    (** The main message. *)
    additional_marks : Span.t list;
    (** Additional marking associated with the main message. *)
    traces : message Span.located bwd;
    (** The backtrace leading to this diagnostic. *)
  }

  (** The severity of a diagnostic. *)
  val severity : t -> Severity.t
end

(** The functor to generate a diagnostic module from an error code module. *)
module Make (C : Code.S) : S with module Code := C
