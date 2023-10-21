open Explication

(** The signature of tags *)
module type Tag = sig
  (** The abstract type of tags. *)
  type t

  (** Comparing whether two tags are equivalent. *)
  val equal : t -> t -> bool

  (** Get the priority number of a tag. We followed the UNIX convention here---a {i smaller} priority number represents higher priority. The convention works well with {!val:List.sort}, which sorts numbers in ascending order. (The more important things go first.) *)
  val priority : t -> int

  (** Ugly printer for debugging *)
  val dump : Format.formatter -> t -> unit
end

(** The signature of explicators. *)
module type S = sig
  module Tag : Tag

  exception Unexpected_end_of_source of Span.position
  (** [Unexpected_end_of_source pos] means the [pos] lies beyond the end of source. This usually means the file has been truncated after the parsing. *)

  exception Unexpected_line_num_increment of Span.position
  (** [Unexpected_line_num_increment pos] means the line number of [pos] is larger than than that of its preceding position during explication, but the explicator did not encounter a newline in between. This usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  exception Unexpected_newline of Span.position
  (** [Unexpected_newline pos] means the line number of [pos] is the same as its preceding position during explication, but the explicator encountered a newline in between. This usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  exception Unexpected_position_in_newline of Span.position
  (** [Unexpected_position_in_newline pos] means the position [pos] is in the middle of a newline. This can happen when the newline consists of multiple bytes, for example [0x0D 0x0A]. It usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  val explicate : ?line_breaking:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> ?blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Span.t) list -> Tag.t t
  (** Explicate a list of spans using content from a data reader.

      @param line_breaking The algorithm to recognize (hard) line breaks. The [`Unicode] algorithm recognizes all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1} as line breaks. The [`Traditional] algorithm only recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] algorithm.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero.
      @param blend The algorithm to blend two tags on a visual span. The default algorithm chooses the more important tag based on priority.

      @raise Unexpected_end_of_source See {!exception:Unexpected_end_of_source}.
      @raise Unexpected_line_num_increment See {!exception:Unexpected_line_num_increment}.
      @raise Unexpected_newline See {!exception:Unexpected_newline}
      @raise Unexpected_position_in_newline See {!Unexpected_position_in_newline}
  *)
end
