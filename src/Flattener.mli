open ExplicationData

type 'style block = (Span.position, 'style) styled list
type 'style part = Span.source * 'style block list

module Make (Style : ExplicatorSigs.Style) : sig
  (* Currently, this can take \tilde{O}(n^2) time where n is the number of styled spans. *)
  val flatten : block_splitting_threshold:int
    -> (Span.t, Style.t) styled list
    -> Style.t part list
end
