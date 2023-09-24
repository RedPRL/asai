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
  (** The type of all message codes. *)
  type t

  (** The default severity of the code. The severity of a message is about whether the message is an error or a warning, etc. To clarify, it is about how serious the message is to the {i end user,} not whether the program should stop or continue. The severity may be overwritten at the time of issuing a message to the end user. *)
  val default_severity : t -> severity

  (** A concise, ideally Google-able string representation of each message code. Detailed or long descriptions of code should be avoided. For example, [E001] works better than [type-checking error]. The shorter, the better. *)
  val to_string : t -> string
end

(** The type of text.

    When we render a diagnostic, the layout engine of the rendering backend should be the one making layout choices. Therefore, we cannot pass already formatted strings. Instead, a text is defined to be a function that takes a formatter and uses it to render the content. The following two conditions must be satisfied:
    + {b All string (and character) literals must be encoded using UTF-8.}
    + {b All string (and character) literals must not contain control characters (such as newlines [\n]).} It is okay to have break hints (such as [@,] and [@ ]) but not literal control characters. This means you should avoid pre-formatted strings, and if you must use them, use {!val:text} to convert newline characters. Control characters include `U+0000-001F` (C0 controls), `U+007F` (backspace), and `U+0080-009F` (C1 controls); in particular, `U+000A` (newline) is a C0 control character. These characters are banned because they would mess up the cursor position. *)
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
