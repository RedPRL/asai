module type S =
sig
  (** The type of message codes. *)
  type t

  (** Get the default severity of the code. *)
  val default_severity : t -> Severity.t

  (** Get the string representation. *)
  val to_string : t -> string
end
