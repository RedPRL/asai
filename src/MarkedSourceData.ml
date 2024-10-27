(** Special positions. *)
type special_position =
  | End_of_line
  | End_of_file

(** A marker is a delimiter of a range or a specific point. *)
type 'tag marker =
  | RangeBegin of 'tag
  | RangeEnd of 'tag
  | Point of 'tag

(** A token is either a string or a marker. *)
type 'tag token =
  | String of string
  | Marker of special_position option * 'tag marker

(** A line is a list of {!type:segment}s along with tags. *)
type 'tag line =
  { markers : 'tag list (** All tags in this line *)
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
