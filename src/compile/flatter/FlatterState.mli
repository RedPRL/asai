type t

val zero : t
val style : t -> Marked.style option
val apply : Marked.style * [`Begin | `End] -> t -> t
