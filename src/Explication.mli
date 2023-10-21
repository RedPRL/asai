(** {1 Types} *)

(* @include *)
include module type of ExplicationData

(** {1 Debugging} *)

(** Ugly printer for debugging *)
val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit
