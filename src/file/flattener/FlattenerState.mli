type t
(** State of the flatterner *)

val zero : t
(** Initial states *)

val style : t -> MarkedDiagnostic.style option
val apply : MarkedDiagnostic.style * [`Begin | `End] -> t -> t
