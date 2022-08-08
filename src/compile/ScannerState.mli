type t

val zero : t
val style : t -> MarkedText.style option
val apply : MarkedText.style * [`Begin | `End] -> t -> t
