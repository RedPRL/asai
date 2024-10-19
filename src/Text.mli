(** {1 Types} *)

(** The type of texts.

    When we render a diagnostic, the layout engine of the diagnostic handler should be the one making layout choices. Therefore, we cannot pass already formatted strings. Instead, a text is defined to be a function that takes a formatter and uses it to render the content. A valid text must satisfy the following two conditions:
    + {b All string (and character) literals must be encoded using UTF-8.}
    + {b All string (and character) literals must not contain control characters (such as the newline character [\n]).} It is okay to have break hints (such as [@,] and [@ ]) but not literal control characters. This means you should avoid pre-formatted strings, and if you must use them, use {!val:text} to convert newline characters. Control characters include `U+0000-001F` (C0 controls), `U+007F` (backspace) and `U+0080-009F` (C1 controls). These characters are banned because they would mess up the cursor position.

    {i Pro-tip:} to format a text in another text, use [%t]:
    {[
      let t = textf "@[<2>this is what the master said:@ @[%t@]@]" inner_text
    ]}
*)
type t = Format.formatter -> unit

(** {1 Builders} *)

(** [make str] converts the string [str] into a text, converting each ['\n'] into a call to {!val:Format.pp_force_newline}. *)
val make : string -> t

(** [makef format ...] formats a text. It is an alias of {!val:Format.dprintf}. Note that there should not be any literal control characters (e.g., literal newline characters). *)
val makef : ('a, Format.formatter, unit, t) format4 -> 'a

(** [kmakef kont format ...] is [kont (makef format ...)]. It is an alias of {!val:Format.kdprintf}. *)
val kmakef : (t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Helper Functions} *)

(** A convenience function that converts a {!type:text} into a string by formatting it with the maximum admissible margin and then replacing newlines and indentation with a space character. *)
val to_string : t -> string
