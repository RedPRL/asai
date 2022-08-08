type t

val empty : t
val add : MarkedText.style * Asai.Span.t -> t -> t
val flatten : threshold:int -> t -> Flattened.blocks
