type highlighting = [ `Primary | `Related ]
type 'a styled = { style : highlighting option; value : 'a }

type t
val empty : t
val singleton : highlighting -> Span.t -> t
val add : highlighting -> Span.t -> t -> t
val flatten : t -> (string * Span.position styled list) list
