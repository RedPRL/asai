type t

val zero : t
val style : t -> MarkedDiagnostic.style option
val apply : MarkedDiagnostic.style * [`Begin | `End] -> t -> t
