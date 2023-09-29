(** {1 Types} *)

(* @include *)
include module type of ExplicationData

(** {1 Helper Functions} *)

val style : 'style -> 'value -> ('value, 'style) styled

(** {1 Debugging} *)

(** Ugly printer for debugging *)
val dump : (Format.formatter -> 'style -> unit) -> Format.formatter -> 'style t -> unit
