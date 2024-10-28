open MarkedSource

type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; markers : (Range.position * 'tag marker) list
  ; line_markers : (int * 'tag) list (* should be sorted by line numbers *)
  }

type 'tag t = (Range.source * 'tag block list) list

val dump_block : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag block -> unit
val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit

module Make (Tag : SourceMarkerSigs.Tag) : sig
  val flatten : block_splitting_threshold:int -> (Range.t * Tag.t) list -> Tag.t t
  (**
     The code needs to handle several subtle cases, expressing in an XML-like notation:
     - The ordering of range markers and point markers at the same location should be ordered like this:
       [...</range1><point/><range2>...]
       [RangeEnd] goes first, and then [Point], and then [RangeBegin].
     - If the set of ranges is "well-scoped" (that is, a range is always completely included in,
       completely including, or being disjoint from another range), then matching beginning and
       ending markers will have the expected nested structures, like this:
       [<range1><range2>...</range2><range3>...</range3></range1>]
     - For two ranges marking the same text with different priorities, the prioritized one goes inside.
       This is to reduce interruption of the prioritized highlighting.
       [<low_priority><high_priority>...</high_priority></low_priority>]
     - For two ranges with the same text and priority, the order of ending markers will follow
       the order of the original input list. This will help the TTY backend display the messages in order.
       [<message2><message1>...</message1></message2>]
  *)
end
