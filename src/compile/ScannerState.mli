type t

val zero : t
val style : t -> MarkedText.style
val apply : [`Start | `End] -> [`Mark | `Highlight] -> t -> t
