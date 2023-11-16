(** Reported reasons for an offset to be invalid. *)
type invalid_offset =
  [ `Negative of int (** Negative offsets *)
  | `Beyond_end_of_file of int * int (** Offsets beyond the end of file *)
  | `Within_newline of int * (int * int) (** Offsets in the middle of a multi-byte newline sequence *)
  ]

(** Reported reasons for a position to be invalid. *)
type invalid_position =
  [ `Offset of invalid_offset (** Invalid offsets *)
  | `Incorrect_start_of_line of int * int (** The start of number is wrong (assuming that the offset is correct). The pair [(m, n)] means that the current [start_of_line] is [m] but the correct value should be [n]. *)
  | `Incorrect_line_num of int * int (** The line number is wrong (assuming that the offset is correct). The pair [(m, n)] means that the current [start_of_line] is [m] but the correct value should be [n]. *)
  ]

(** Reported reasons for a range to be invalid. *)
type invalid_range =
  [ `Begin of invalid_position (** The first position of a range is invalid. *)
  | `End of invalid_position (** The second position of a range is invalid. *)
  | `End_of_file of invalid_position (** The range is a special end-of-file marking, but the position is invalid. *)
  | `Not_end_of_file of int * int (** The range is a special end-of-file marking, but the offset of the position is not at the end of file. The pair [(m, n)] means that the current offset is [m] but the length or size of the source is [n]. *)
  ]
