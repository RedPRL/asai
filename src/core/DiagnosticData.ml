open Bwd

type severity =
  | Hint
  | Info
  | Warning
  | Error
  | Bug

let string_of_severity =
  function
  | Hint -> "Hint"
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
  | Bug -> "Bug"

module type Code =
sig
  (** The type of message codes. *)
  type t

  (** Get the default severity of the code. *)
  val default_severity : t -> severity

  (** Get the string representation. *)
  val to_string : t -> string
end

(** The type of single messages.

    When we render a diagnostic, the layout engine of the rendering backend should be the one making layout choices. Therefore, we cannot pass already formatted strings but a function awaiting a formatter. This is best paired with {!val:Format.dprintf}, which allows us to delay formatting choices. *)
type message = Format.formatter -> unit

(** The type of diagnostics. *)
type 'code t = {
  severity : severity;
  (** Severity of the diagnostic. *)
  code : 'code;
  (** The message code. *)
  message : message Span.located;
  (** The main message. *)
  additional_marks : Span.t list;
  (** Additional marking associated with the main message. *)
  backtrace : message Span.located bwd;
  (** The backtrace leading to this diagnostic. *)
}

(** Mapping the code *)
let map f d = {d with code = f d.code}
