type block = Span.position Explicated.styled list
type section = string * block list

type 'a marked =
  { value : 'a
  ; marks : section list
  }

val flatten : splitting_threshold:int -> additional_marks:Span.t list -> Span.t option -> section list
