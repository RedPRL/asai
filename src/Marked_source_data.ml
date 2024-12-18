type special_position = [`End_of_line | `End_of_file]

(** A mark is a delimiter of a range or a specific point. *)
type 'tag mark =
  | Range_begin of 'tag
  | Range_end of 'tag
  | Point of 'tag

(** A token is either a string or a mark. *)
type 'tag token =
  | String of string
  | Mark of special_position option * 'tag mark

(** A line is a list of {!type:token}s along with marks. *)
type 'tag line =
  { marks : 'tag list (** All tags attached to this line *)
  ; tokens : 'tag token list
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
