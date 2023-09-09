(* @include *)
include module type of DiagnosticData

(** A convenience function that turns severities into strings. *)
val string_of_severity : severity -> string

(** A convenience function that maps the message code. This is helpful when using {!val:Logger.S.adopt}. *)
val map : ('code1 -> 'code2) -> 'code1 t -> 'code2 t

(** [append_marks msg marks] appends [marks] to the additional marks of [msg]. *)
val append_marks : 'code t -> Span.t list -> 'code t
