module type S =
sig
  (** The type of trace phases. *)
  type t

  (** Get the string representation. *)
  val to_string : t -> string
end
