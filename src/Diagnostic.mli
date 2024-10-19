(** {1 Types} *)

(* @include *)
include module type of DiagnosticData

(** {1 Constructing Diagnostics} *)

(** [of_text severity message text] constructs a diagnostic from a {!type:Text.t}.

    Example:
    {[
      of_text Warning ChiError @@ text "your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.

    @since 0.2.0
*)
val of_text : ?loc:Range.t -> ?backtrace:backtrace -> ?extra_remarks:Loctext.t list -> severity -> 'message -> Text.t -> 'message t

(** [of_loctext severity message loctext] constructs a diagnostic from a {!type:Loctext.t}.

    Example:
    {[
      of_loctext Warning ChiError @@ loctext "your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val of_loctext : ?backtrace:backtrace -> ?extra_remarks:Loctext.t list -> severity -> 'message -> Loctext.t -> 'message t

(** [make severity message loctext] constructs a diagnostic with the [loctext].

    Example:
    {[
      make Warning ChiError "your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val make : ?loc:Range.t -> ?backtrace:backtrace -> ?extra_remarks:Loctext.t list -> severity -> 'message -> string -> 'message t

(** [makef severity message format ...] is [of_loctext severity message (Loctext.makef format ...)]. It formats the message and constructs a diagnostic out of it.

    Example:
    {[
      makef Warning ChiError "your %s is critically low" "Ch'i"
    ]}

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val makef : ?loc:Range.t -> ?backtrace:backtrace -> ?extra_remarks:Loctext.t list -> severity -> 'message -> ('a, Format.formatter, unit, 'message t) format4 -> 'a

(** [kmakef kont severity message format ...] is [kont (makef severity message format ...)].

    @param loc The location of the text (usually the code) to highlight.
    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val kmakef : ?loc:Range.t -> ?backtrace:backtrace -> ?extra_remarks:Loctext.t list -> ('message t -> 'b) -> severity -> 'message -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {1 Other Helper Functions} *)

(** A convenience function that maps the message of a diagnostic. This is helpful when using {!val:Reporter.S.adopt}. *)
val map : ('message1 -> 'message2) -> 'message1 t -> 'message2 t

(** {1 Deprecated Types and Functions} *)

(** An alias of [Text.t] for backward compatibility. *)
type text = Text.t
[@@ocaml.alert deprecated "Use Text.t instead"]

(** An alias of [Loctext.t] for backward compatibility. *)
type loctext = Loctext.t
[@@ocaml.alert deprecated "Use Loctext.t instead"]

(** An alias of [Text.make] for backward compatibility. *)
val text : string -> Text.t
[@@ocaml.alert deprecated "Use Text.make instead"]

(** An alias of [Text.makef] for backward compatibility. *)
val textf : ('a, Format.formatter, unit, Text.t) format4 -> 'a
[@@ocaml.alert deprecated "Use Text.makef instead"]

(** An alias of [Text.kmakef] for backward compatibility. *)
val ktextf : (Text.t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
[@@ocaml.alert deprecated "Use Text.kmakef instead"]

(** An alias of [Loctext.make] for backward compatibility. *)
val loctext : ?loc:Range.t -> string -> Loctext.t
[@@ocaml.alert deprecated "Use Loctext.make instead"]

(** An alias of [Loctext.makef] for backward compatibility. *)
val loctextf : ?loc:Range.t -> ('a, Format.formatter, unit, Loctext.t) format4 -> 'a
[@@ocaml.alert deprecated "Use Loctext.makef instead"]

(** An alias of [Loctext.kmakef] for backward compatibility. *)
val kloctextf : ?loc:Range.t -> (Loctext.t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
[@@ocaml.alert deprecated "Use Loctext.kmakef instead"]

(** An alias of [Text.to_string] for backward compatibility. *)
val string_of_text : Text.t -> string
[@@ocaml.alert deprecated "Use Text.to_string instead"]
