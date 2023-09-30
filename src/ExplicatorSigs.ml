open Explication

(** The signature of data readers. *)
module type Reader =
sig
  (** An abstract type of files. *)
  type file

  (** [load file_path] loads the resource at [file_path]. *)
  val load : string -> file

  (** [length file_path] gets the size of the file. *)
  val length : file -> int

  (** [unsafe_get file_path i] reads the ith byte of the file without checking the file size. *)
  val unsafe_get : file -> int -> char
end

(** The signature of highlighting styles *)
module type Style = sig
  (** The abstract type of highlighting styles. *)
  type t

  (** [default] is the default style, meaning no highlighting is applied. It should be the unit of [compose]. *)
  val default : t

  (** Comparing whether two styles are equivalent. *)
  val equal : t -> t -> bool

  (** Comparing styles for the importance they represent. *)
  val compare : t -> t -> int

  (** [max s1 s2] gives the style that signals more importance. *)
  val max : t -> t -> t

  (** [is_default s] checks if the style is the default style. It should be equivalent to [equal default s]. *)
  val is_default : t -> bool

  (** Compose two styles into one. The operator should form a commutative group with {!val:default} being its unit. *)
  val compose : t -> t -> t

  (** Ugly printer for debugging *)
  val dump : Format.formatter -> t -> unit
end

(** The signature of explicators. *)
module type S = sig
  module Style : Style

  exception Unexpected_end_of_file of Span.position
  (** [Unexpected_end_of_file pos] means the [pos] lies beyond the end of file. This usually means the file has been truncated after the parsing. *)

  exception Unexpected_line_num_increment of Span.position
  (** [Unexpected_line_num_increment pos] means the line number of [pos] is larger than than that of its preceding position during explication, but the explicator did not encounter a newline in between. This usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  exception Unexpected_newline of Span.position
  (** [Unexpected_newline pos] means the line number of [pos] is the same as its preceding position during explication, but the explicator encountered a newline in between. This usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  exception Unexpected_position_in_newline of Span.position
  (** [Unexpected_position_in_newline pos] means the position [pos] is in the middle of a newline. This can happen when the newline consists of multiple bytes, for example [0x0D 0x0A]. It usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  val explicate : ?line_breaking:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> (Span.t, Style.t) styled list -> Style.t t
  (** Explicate a list of spans using content from a data reader.

      @param line_breaking The algorithm to recognize (hard) line breaks. The [`Unicode] algorithm recognizes all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1} as line breaks. The [`Traditional] algorithm only recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] algorithm.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero.

      @raise UnexpectedLineNumIncrement if the line number of some position is increased by one but there was no newline character [\n].
      @raise PositionBeyondEndOfFile if some position falls outside the data content. (That is, the file is too smaller, if the data reader is reading files.)
  *)
end
