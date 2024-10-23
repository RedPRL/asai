open Explication

type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; markers : (Range.position * 'tag marker) list
  ; line_markers : (int * 'tag) list (* should be sorted by line numbers *)
  }

type 'tag t = (Range.source * 'tag block list) list

val dump_block : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag block -> unit
val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit

module Make (Tag : ExplicatorSigs.Tag) : sig
  val flatten : block_splitting_threshold:int -> (Range.t * Tag.t) list -> Tag.t t
end
