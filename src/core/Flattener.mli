type t
val empty : t
val singleton : Context.highlighting -> Span.t -> t
val add : Context.highlighting -> Span.t -> t -> t
val flatten : t -> (string * Span.position Context.styled list) list
