type t

val empty : t
val add : MarkedDiagnostic.style * Asai.Span.t -> t -> t
val singleton : MarkedDiagnostic.style * Asai.Span.t -> t
val flatten : splitting_threshold:int -> t -> Flattened.section list
