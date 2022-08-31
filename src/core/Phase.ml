module type S =
sig
  (** The type of tace tags. *)
  type t

  (** Get the string representation. *)
  val to_string : t -> string
end
