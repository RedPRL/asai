type 'style block = (Span.position, 'style) ExplicatorData.styled list
type 'style part = string * 'style block list

module Make (Style : ExplicatorSigs.Style) : sig
  val flatten : splitting_threshold:int
    -> (Span.t, Style.t) ExplicatorData.styled list
    -> Style.t part list
end
