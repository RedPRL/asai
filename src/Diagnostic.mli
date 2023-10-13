(** {1 Types} *)

(* @include *)
include module type of DiagnosticData

(** {1 Constructing Messages} *)

(** [text str] converts the string [str] into a text, converting each ['\n'] into a call to {!val:Format.pp_force_newline}. *)
val text : string -> text

(** [textf format ...] constructs a text. It is an alias of {!val:Format.dprintf}. Note that there should not be any literal control characters (e.g., literal newline characters). *)
val textf : ('a, Format.formatter, unit, text) format4 -> 'a

(** [ktextf kont format ...] is [kont (textf format ...)]. It is an alias of {!val:Format.kdprintf}. *)
val ktextf : (text -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** [loctext str] converts the string [str] into a loctext.

    @param loc The location of the loctext (usually the code) to highlight. *)
val loctext : ?loc:Span.t -> string -> loctext

(** [loctextf format ...] constructs a loctext. Note that there should not be any literal control characters (e.g., literal newline characters).

    @param loc The location of the loctext (usually the code) to highlight.
*)
val loctextf : ?loc:Span.t -> ('a, Format.formatter, unit, loctext) format4 -> 'a

(** [kloctextf kont format ...] is [kont (loctextf format ...)].

    @param loc The location of the loctext (usually the code) to highlight.
*)
val kloctextf : ?loc:Span.t -> (loctext -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Constructing Diagnostics} *)

(** [of_text severity message text] constructs a diagnostic from a {!type:text}.

    Example:
    {[
      of_text Warning ChiError @@ text "Your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.

    @since 0.2.0
*)
val of_text : ?loc:Span.t -> ?backtrace:backtrace -> ?extra_remarks:loctext list -> severity -> 'message -> text -> 'message t

(** [of_loctext severity message loctext] constructs a diagnostic from a {!type:loctext}.

    Example:
    {[
      of_loctext Warning ChiError @@ loctext "Your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val of_loctext : ?backtrace:backtrace -> ?extra_remarks:loctext list -> severity -> 'message -> loctext -> 'message t

(** [make severity message loctext] constructs a diagnostic with the [loctext].

    Example:
    {[
      make Warning ChiError "Your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val make : ?loc:Span.t -> ?backtrace:backtrace -> ?extra_remarks:loctext list -> severity -> 'message -> string -> 'message t

(** [makef severity message format ...] is [of_loctext severity message (loctextf format ...)]. It formats the message and constructs a diagnostic out of it.

    Example:
    {[
      makef Warning ChiError "Your %s is critically low" "Ch'i"
    ]}

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val makef : ?loc:Span.t -> ?backtrace:backtrace -> ?extra_remarks:loctext list -> severity -> 'message -> ('a, Format.formatter, unit, 'message t) format4 -> 'a

(** [kmakef kont severity message format ...] is [kont (makef severity message format ...)].

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val kmakef : ?loc:Span.t -> ?backtrace:backtrace -> ?extra_remarks:loctext list -> ('message t -> 'b) -> severity -> 'message -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Other Helper Functions} *)

(** A convenience function that maps the message of a diagnostic. This is helpful when using {!val:Reporter.S.adopt}. *)
val map : ('message1 -> 'message2) -> 'message1 t -> 'message2 t

(** A convenience function that converts a {!type:severity} into its constructor name. For example, {!constructor:Warning} will be converted into the string ["Warning"]. *)
val string_of_severity : severity -> string

(** A convenience function that converts a {!type:text} into a string by formatting it with the maximum admissible margin. Note that the resulting string may contain newline characters. *)
val string_of_text : text -> string
