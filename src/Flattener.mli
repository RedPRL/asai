type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; tagged_positions : ('tag option * Range.position) list
  ; tagged_lines : ('tag * int) list
  }

type 'tag t = (Range.source * 'tag block list) list

val dump_block : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag block -> unit

val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit

module Make (Tag : ExplicatorSigs.Tag) : sig
  val flatten : block_splitting_threshold:int -> blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Range.t) list -> Tag.t t
end
