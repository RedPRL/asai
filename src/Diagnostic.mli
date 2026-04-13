(** {1 Types} *)

open Bwd

(* @include *)
include module type of Diagnostic_data

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
val of_text : ?loc:Range.t -> ?backtrace:'explanation bwd -> ?extra_remarks:'explanation list -> severity -> 'message -> Text.t -> ('message, 'explanation) t

(** [of_explanation severity message explanation] constructs a diagnostic from an explanation.

    Example:
    {[
      of_explanation Warning ChiError @@ Loctext.make "your Ch'i is critically low"
    ]}

    @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
    @param extra_remarks Additional remarks that are not part of the backtrace.
*)
val make : ?backtrace:'explanation bwd -> ?extra_remarks:'explanation list -> severity -> 'message -> 'explanation -> ('message, 'explanation) t

(** {1 Other Helper Functions} *)

(** A convenience function that maps the message of a diagnostic. This is helpful when using {!val:Reporter.S.adopt}. *)
val map : ('message1 -> 'message2) -> ('explanation1 -> 'explanation2) -> ('message1, 'explanation1) t -> ('message2, 'explanation2) t

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
