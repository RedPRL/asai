(** {1 Types} *)

(* @include *)
include module type of DiagnosticData

(** {1 Utility Functions} *)

(** A convenience function that turns severities into strings. *)
val string_of_severity : severity -> string

(** A convenience function that turns a message into a string by formatting it with the maximum admissible margin. *)
val string_of_message : message -> string

(** A convenience function that maps the message code. This is helpful when using {!val:Logger.S.adopt}. *)
val map : ('code1 -> 'code2) -> 'code1 t -> 'code2 t

(** [append_marks msg marks] appends [marks] to the additional marks of [msg]. *)
val append_marks : 'code t -> Span.t list -> 'code t
