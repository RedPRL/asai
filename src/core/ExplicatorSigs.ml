open ExplicatorData

(** The signature of effects-based data readers. *)
module type Reader =
sig
  (** [length file_path] gets the size of the file. *)
  val length : string -> int

  (** [unsafe_get file_path i] reads the ith byte of the file without checking the file size. *)
  val unsafe_get : string -> int -> char

  (** [run f] runs the thunk [f] and handles the internal algebraic effects. *)
  val run : (unit -> 'a) -> 'a
end

(** The signature of highlighting styles *)
module type Style = sig
  (** The abstract type of highlighting styles. *)
  type t

  (** [none] is the default style, meaning no highlighting is applied. It should be the unit of [compose]. *)
  val none : t

  (** Comparing whether two styles are equivalent. *)
  val equal : t -> t -> bool

  (** Comparing styles for the importance they represent. *)
  val compare : t -> t -> int

  (** [max s1 s2] gives the style that signals more importance. *)
  val max : t -> t -> t

  (** [is_none s] checks if the style is the default style. It should be the same as [equal none s]. *)
  val is_none : t -> bool

  (** Compose two styles into one. The operator should form a commutative group with {!val:none} being its unit. *)
  val compose : t -> t -> t
end

module type S = sig
  module Style : Style

  val explicate : ?splitting_threshold:int -> (Span.t, Style.t) styled list -> Style.t explication
  (** Explicate a span using content from the reader.

      @param splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero. *)
end
