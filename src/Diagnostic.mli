(** {1 Types} *)

(* @include *)
include module type of DiagnosticData

(** {1 Constructing Messages} *)

(** [text str] converts the string [str] into a text, converting each ['\n'] into a call to {!val:Format.pp_force_newline}. *)
val text : string -> text

(** [textf format ...] constructs a text. It is an alias of {!val:Format.dprintf}. Note that there should not be any literal control characters (e.g., literal newline characters). *)
val textf : ('a, Format.formatter, unit, text) format4 -> 'a

(** [ktextf kont format ...] is [kont (textf code format ...)]. It is an alias of {!val:Format.kdprintf}. *)
val ktextf : (text -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** [message str] converts the string [str] into a message.

    @param loc The location of the message (usually the code) to highlight. *)
val message : ?loc:Span.t -> string -> message

(** [messagef format ...] constructs a message. Note that there should not be any literal control characters (e.g., literal newline characters).

    @param loc The location of the message (usually the code) to highlight.
*)
val messagef : ?loc:Span.t -> ('a, Format.formatter, unit, message) format4 -> 'a

(** [kmessagef kont format ...] is [kont (messagef code format ...)].

    @param loc The location of the message (usually the code) to highlight.
*)
val kmessagef : ?loc:Span.t -> (message -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Constructing Diagnostics} *)

(** [make severity code str] constructs a diagnostic with the message [str].

    Example:
    {[
      make Warning `ChiError "Your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
*)
val make : ?loc:Span.t -> ?backtrace:backtrace -> ?additional_messages:message list -> severity -> 'code -> string -> 'code t

(** [makef severity code format ...] is [make severity code (messagef format ...)]. It formats the message and constructs a diagnostic out of it.

    Example:
    {[
      makef Warning `ChiError "Your %s is critically low" "Ch'i"
    ]}

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
*)
val makef : ?loc:Span.t -> ?backtrace:backtrace -> ?additional_messages:message list -> severity -> 'code -> ('a, Format.formatter, unit, 'code t) format4 -> 'a

(** [kmakef kont severity code format ...] is [kont (makef severity code format ...)].

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
*)
val kmakef : ?loc:Span.t -> ?backtrace:backtrace -> ?additional_messages:message list -> ('code t -> 'b) -> severity -> 'code -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** [of_message severity code message] constructs a diagnostic with the [message].

    Example:
    {[
      make Warning `ChiError @@ message "Your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
*)
val of_message : ?backtrace:backtrace -> ?additional_messages:message list -> severity -> 'code -> message -> 'code t

(** {1 Other Helper Functions} *)

(** A convenience function that maps the message code of a diagnostic. This is helpful when using {!val:Reporter.S.adopt}. *)
val map : ('code1 -> 'code2) -> 'code1 t -> 'code2 t

(** A convenience function that maps the message text of a diagnostic.

    Example:
    {[
      let d2 = map_text (textf "@[<2>Pluto is no longer a planet:@ %t@]") d1
    ]}

    @since 0.2.0
*)
val map_text : (text -> text) -> 'a t -> 'a t

(** A convenience function that converts a {!type:severity} into its constructor name. For example, {!constructor:Warning} will be converted into the string ["Warning"]. *)
val string_of_severity : severity -> string

(** A convenience function that converts a {!type:text} into a string by formatting it with the maximum admissible margin. Note that the resulting string may contain control characters and might not be suitable for constructing new instances of {!type:text} or {!type:message}. *)
val string_of_text : text -> string
