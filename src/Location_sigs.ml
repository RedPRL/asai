(** An auxiliary type to package data with an optional location. *)
type ('data, 'location) located = { value : 'data; loc : 'location option }

module type S =
sig
  type t
  val to_range : t -> Range.t option
  val pp : Format.formatter -> t -> unit
end
