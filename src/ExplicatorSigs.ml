open Explication

exception Invalid_range of Range.t * UserContent.invalid_range
(** [Invalid_range (range, reason)] means that [range] is an invalid range because of [reason]. This exception will be raised only when the debug mode is enabled. See the [debug] argument of {!val:Explicator.S.explicate}. *)

(** The signature of tags *)
module type Tag = sig
  (** The abstract type of tags. *)
  type t

  (** Get the priority number of a tag. We followed the UNIX convention here---a {i smaller} priority number represents higher priority. The convention works well with {!val:List.sort}, which sorts numbers in ascending order. (The more important things go first.) *)
  val priority : t -> int

  (** Ugly printer for debugging *)
  val dump : Format.formatter -> t -> unit
end

(** The signature of explicators. *)
module type S = sig
  module Tag : Tag

  val explicate : ?line_breaks:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> ?blend:(Tag.t -> Tag.t -> Tag.t) -> ?debug:bool -> (Tag.t * Range.t) list -> Tag.t t
  (** Explicate a list of ranges using content from a data reader. This function must be run under [SourceReader.run].

      @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero.
      @param blend The algorithm to blend two tags on a visual range. The default algorithm chooses the more important tag based on priority.
      @param debug Whether to enable the debug mode that performs expensive extra checking. The default is [false].

      @raise Invalid_range See {!exception:Invalid_range}.
  *)
end
