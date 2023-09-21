(** {1 Types} *)

(* @include *)
include module type of DiagnosticData

(** {1 Constructions} *)

(** [textf format ...] constructs a text. It is an alias of {!val:Format.dprintf}. *)
val textf : ('a, Format.formatter, unit, text) format4 -> 'a

(** [ktextf kont format ...] is [kont (textf code format ...)]. It is an alias of {!val:Format.kdprintf}. *)
val ktextf : (text -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** [messagef format ...] constructs a message.

    @param loc The location of the message (usually the code) to highlight.
*)
val messagef : ?loc:Span.t -> ('a, Format.formatter, unit, message) format4 -> 'a

(** [kmessagef kont format ...] is [kont (messagef code format ...)].

    @param loc The location of the message (usually the code) to highlight.
*)
val kmessagef : (message -> 'b) -> ?loc:Span.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Utility Functions} *)

(** A convenience function that turns a {!type:severity} into a string. *)
val string_of_severity : severity -> string

(** A convenience function that turns a {!type:text} into a string by formatting it with the maximum admissible margin. Note that the resulting string may contain control characters and might not be suitable for constructing another instance of {!type:text} or {!type:message}. *)
val string_of_text : text -> string

(** A convenience function that maps the message code. This is helpful when using {!val:Logger.S.adopt}. *)
val map : ('code1 -> 'code2) -> 'code1 t -> 'code2 t
