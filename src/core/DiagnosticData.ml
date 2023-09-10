open Bwd

(** The type of severity. *)
type severity =
  | Hint
  | Info
  | Warning
  | Error
  | Bug

(** The signature of message code. An implementer should specify the message code used in their library or application. *)
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

    When we render a diagnostic, the layout engine of the rendering backend should be the one making layout choices. Therefore, we cannot pass already formatted strings. Instead, a message is defined to be a function that takes a formatter and uses it to render the content. *)
type message = Format.formatter -> unit

(** The type of frames in a backtrace. *)
type frame = message Span.located

(** The type of backtraces, stored as backward lists of frames. *)
type backtrace = frame bwd

(** The type of diagnostics. *)
type 'code t = {
  severity : severity;
  (** Severity of the diagnostic. *)
  code : 'code;
  (** The message code. *)
  message : message Span.located;
  (** The main message. *)
  additional_marks : Span.t list;
  (** Additional code fragments that are relevant to the main message. *)
  backtrace : backtrace;
  (** The backtrace leading to this diagnostic. *)
}
