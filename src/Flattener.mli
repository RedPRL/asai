type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; tagged_positions : ('tag option * Span.position) list
  ; tagged_lines : ('tag * int) list
  }

type 'tag t = (Span.source * 'tag block list) list

val dump_block : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag block -> unit

val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit

module Make (Tag : ExplicatorSigs.Tag) : sig
  (* Currently, this can take \tilde{O}(n^2) time where n is the number of tagged spans. *)
  val flatten : block_splitting_threshold:int -> blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Span.t) list -> Tag.t t
end
