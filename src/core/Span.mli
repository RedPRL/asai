(** The type of locations and single spans.

    All positions are byte-oriented. Here are some popular alternatives:
    + Unicode characters (which may not match user-perceived characters)
    + Unicode grapheme clusters (user-perceived characters; see the {{: https://erratique.ch/software/uuseg}uuseg} library)
    + Column numbers (visual width of a string in display)

    It takes at least linear time to count Unicode characters (except when UTF-32 is in use) or Unicode grapheme clusters from raw bytes. Column numbers are even worse---they are not well-defined: The visual column width of a string highly depends on the eventual renderer (e.g., web browsers, text editors, or terminal emulators) and there is no reliable protocol to communicate the active rendering convention. The rendering result can depend on the fonts (e.g., whether certain emoji sequences are supported, and how CJK characters whose East Asian width is "Ambiguous" are rendered), the locale (the data in [LC_CTYPE]), and other settings (such as the tab size). Many applications use the heuristics implemented in the POSIX function [wcswidth], ignoring all the tricky cases. We use only semantically well-defined units. In sum, we believe {e bytes} are the only efficient and portable position unit. *)

(** {1 Types} *)

(** The type of positions *)
type position = {
  file_path : string;
  (** The absolute file path of the file that contains the position. *)

  offset : int;
  (** The byte offset of the position relative to the beginning of the file. *)

  start_of_line : int;
  (** The byte offset pointing to the start of the line that contains the position. *)

  line_num : int;
  (** The 1-indexed line number of the line that contains the position. *)
}

(** The abstract type of spans. *)
type t

(** {1 Builders} *)

(** [make beginning ending] builds the span [\[begining, ending)] (not including the byte at the ending position) in from a pair of positions [beginning] and [ending].

    @raise Invalid_argument if the positions do not share the same file path or if [end_] comes before [begin_]. The comparison of file paths is done by [String.equal] without any path normalization.
*)
val make : position -> position -> t

(** [of_lex_pos pos] conversion [pos] of type {!type:Lexing.position} to a {!type:position}. The input [pos] must be in byte-indexed. (Therefore, [ocamllex] is compatible, but [sedlex] is not because it uses code points.) *)
val of_lex_pos : Lexing.position -> position

(** [to_positions span] returns the pair of the beginning and ending positions of [span]. *)
val to_positions : t -> position * position

(** {1 Accessors} *)

(** [file_path span] returns the file path associated with [span]. *)
val file_path : t -> string

(** [begin_line_num span] returns the 1-indexed line number of the beginning position. *)
val begin_line_num : t -> int

(** [end_line_num span] returns the 1-indexed line number of the ending position. *)
val end_line_num : t -> int

(** {1 Auxiliary types} *)

type 'a located = { loc : t option; value : 'a }
