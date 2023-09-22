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

  (** The default severity of the code. *)
  val default_severity : t -> severity

  (** The concise, Google-able string representation of a code. Detailed descriptions of code should be avoided.
      For example, [E001] works better than [type-checking error]. *)
  val to_string : t -> string
end

(** The type of text.

    When we render a diagnostic, the layout engine of the rendering backend should be the one making layout choices. Therefore, we cannot pass already formatted strings. Instead, a text is defined to be a function that takes a formatter and uses it to render the content. The text itself should not contain literal control characters (such as newlines) but only break hints (such as [@,]). Control characters include `U+0000-001F` (C0 controls), `U+007F` (backspace), and `U+0080-009F` (C1 controls); in particular, `U+000A` (newline) is a C0 control character. These characters are banned because they would mess up the cursor position. *)
type text = Format.formatter -> unit

(** A message is a located {!type:text}. *)
type message = text Span.located

(** A backtrace is a (backward) list of messages. *)
type backtrace = message bwd

(** The type of diagnostics. *)
type 'code t = {
  severity : severity;
  (** Severity of the diagnostic. *)
  code : 'code;
  (** The message code. *)
  message : message;
  (** The main message. *)
  backtrace : backtrace;
  (** The backtrace leading to this diagnostic. *)
  additional_messages : message list;
  (** Additional messages relevant to the main message that are not part of the backtrace. *)
}
