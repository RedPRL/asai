type t

val empty : t
val add : Marked.style * Asai.Span.t -> t -> t
val singleton : Marked.style * Asai.Span.t -> t
val flatten : splitting_threshold:int -> t -> Flattened.section list
