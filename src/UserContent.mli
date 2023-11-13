(** [find_eol ~line_breaks read (pos, eof)] returns the end position of the first line and the length of the first newline sequence (if any) within the range [\[pos, end)]. If no newlines are found, then [None] is returned as the length.

    @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
    @param read The function to read the character at a position.
    @param eof The length of the source. *)
val find_eol : line_breaks:[`Unicode | `Traditional] -> (int -> char) -> int * int -> int * int option

(** [count_newlines ~line_breaks read (pos, eof)] counts the number of newlines within the range [\[pos, end)].

    @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
    @param read The function to read the character at a position.
    @param eof The length of the source. *)
val count_newlines : line_breaks:[`Unicode | `Traditional] -> (int -> char) -> int * int -> int

(** [check_line_num ~line_breaks read range] checks the line numbers in the [range].

    @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
    @param read The function to read the character at a position. *)
val check_line_num : line_breaks:[`Unicode | `Traditional] -> (int -> char) -> Range.t -> bool

(** [replace_control str] replaces control characters and newline sequences in [str] with the replacement character [Uchar.rep]. *)
val replace_control : tab_size:int -> string -> string
