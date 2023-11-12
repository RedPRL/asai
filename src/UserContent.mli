(** [find_eol read (pos, eof)] returns the end position of the first line and the length of the first newline sequence (if any) within the range [pos, end). If no newlines are found, then [None] is returned as the length.

    @param line_breaking the algorithm to detect newline sequences.
    @param read the function to read the character at a position.
    @param eof the length of the source. *)
val find_eol : line_breaking:[`Unicode | `Traditional] -> (int -> char) -> int * int -> int * int option

(** [count_newlines read (pos, eof)] counts the number of newlines within the range [pos, end).

    @param line_breaking the algorithm to detect newline sequences.
    @param read the function to read the character at a position.
    @param eof the length of the source. *)
val count_newlines : line_breaking:[`Unicode | `Traditional] -> (int -> char) -> int * int -> int

(** [replace_control str] replaces control characters and newline sequences in [str] with the replacement character [Uchar.rep]. *)
val replace_control : tab_size:int -> string -> string
