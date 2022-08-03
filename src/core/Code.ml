module type S =
sig
  (** The type of message codes. *)
  type t

  (** Get the severity of the code. *)
  val severity : t -> Severity.t

  (** Get the string representation. *)
  val to_string : t -> string

  (** Get a long-form description of a code. *)
  val description : t -> string
end
