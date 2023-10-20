module Make (Tag : ExplicatorSigs.Tag) : sig
  type block =
    { begin_line_num : int
    ; end_line_num : int
    ; tagged_positions : (Tag.t option * Span.position) list
    ; tagged_lines : (Tag.t * int) list
    }

  (* Currently, this can take \tilde{O}(n^2) time where n is the number of tagged spans. *)
  val flatten : block_splitting_threshold:int -> blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Span.t) list -> (Span.source * block list) list
end
