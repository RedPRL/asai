include module type of DiagnosticData

val string_of_severity : severity -> string
val map : ('code1 -> 'code2) -> 'code1 t -> 'code2 t
