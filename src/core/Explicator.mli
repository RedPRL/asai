(** @inline *)
include module type of Explicated

(** {1 Explication} *)

(** The signature of a generic effects-based reader. *)
module type Reader = Reader.S

(** Making an explicator. *)
module Make : Reader ->
  sig
    val explicate : ?splitting_threshold:int -> 'code Diagnostic.t -> 'code Explicated.diagnostic
    (** Explicate a diagnostic using content from the reader.

        @param splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is zero. *)
  end
