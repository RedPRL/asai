module type S =
sig
  (** The type of message codes. *)
  type t

  (** Get the default_severity of the code. *)
  val default_severity : t -> Severity.t

  (** Get the string representation. *)
  val to_string : t -> string
end
