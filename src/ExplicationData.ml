(** A segment is an optionally tagged string from the user content. (Note the use of [option].) *)
type 'tag segment = 'tag option * string

(** A line is a list of {!type:segment}s along with tags. *)
type 'tag line =
  { tags : 'tag list
  ; segments : 'tag segment list
  }

(** A block is a collection of consecutive lines. *)
type 'tag block =
  { begin_line_num : int (** The starting 1-indexed line number of a block. *)
  ; end_line_num : int (** The ending 1-indexed line number of a block. *)
  ; lines : 'tag line list (** The {!type:line}s within a block. *)
  }

(** A part consists of multiple blocks from the same file. These blocks should be non-overlapping and sorted by importance or the textual order. *)
type 'tag part =
  { source : Range.source (** The source of a part. *)
  ; blocks : 'tag block list (** The blocks within a part. *)
  }

(** Highlighted texts. *)
type 'tag t = 'tag part list
