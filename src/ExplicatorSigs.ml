open ExplicatorData

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
end

(** The signature of explicators. *)
module type S = sig
  module Style : Style

  exception UnexpectedLineNumIncrement of Span.position
  (** [UnexpectedLineNumIncrement pos] means the line number of [pos] is larger than than that of its preceding position, but the explicator did not encounter a newline character [\n] in between. This usually indicates that there's something wrong with the lexer, or that the file has changed since the parsing. *)

  exception PositionBeyondEndOfFile of Span.position
  (** [PositionBeyondEndOfFile pos] means the [pos] lies beyond the end of file. This usually means the file has been truncated after the parsing. *)

  val explicate : ?line_breaks:[`Unicode | `Traditional] -> ?splitting_threshold:int -> (Span.t, Style.t) styled list -> Style.t explication
  (** Explicate a list of spans using content from a data reader.

      @param line_breaks The set of line breaks that should be recognized. The [`Unicode] set recognizes all Unicode character sequences in Unicode 15.0.0 Table 5-1. The [`Traditional] set recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
      @param splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero.

      @raise UnexpectedLineNumIncrement if the line number of some position is increased by one but there was no newline character [\n].
      @raise PositionBeyondEndOfFile if some position falls outside the data content. (That is, the file is too smaller, if the data reader is reading files.)
  *)
end
