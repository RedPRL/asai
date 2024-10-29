(** {1 Types} *)

(* @include *)
include module type of Marked_source_data

(** {1 Debugging} *)

(** Ugly printer for {!type:mark} *)
val dump_mark : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag mark -> unit

(** Ugly printer for {!type:t} *)
val dump : (Format.formatter -> 'tag -> unit) -> Format.formatter -> 'tag t -> unit
