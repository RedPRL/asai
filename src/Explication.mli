(** {1 Types} *)

(* @include *)
include module type of ExplicationData

(** {1 Debugging} *)

(** Ugly printer for {!type:marker} *)
val dump_marker : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag marker -> unit

(** Ugly printer for {!type:t} *)
val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit
