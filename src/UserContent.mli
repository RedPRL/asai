include module type of UserContentData

(** The exception indicating that a range is invalid. *)
exception Invalid_range of invalid_range

(** [find_eol ~line_breaks read (pos, eof)] returns the end position of the first line and the length of the first newline sequence (if any) within the range [\[pos, end)]. If no newlines are found, then [None] is returned as the length.

    @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
    @param read The function to read the character at a position.
    @param eof The length of the source. *)
val find_eol : line_breaks:[`Unicode | `Traditional] -> (int -> char) -> int * int -> int * int option

(** [check_range ~line_breaks ~eof read range] checks whether [range] is valid.

    @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
    @param read The function to read the character at a position.

    @raise Invalid_range if range is invalid. See the documentation of {!exception:Invalid_range}. *)
val check_range : line_breaks:[`Unicode | `Traditional] -> eof:int -> (int -> char) -> Range.t -> unit

(** [replace_control str] replaces control characters and newline sequences in [str] with the replacement character [Uchar.rep]. *)
val replace_control : tab_size:int -> string -> string
