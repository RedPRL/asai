module type S =
sig
  (** The type of message codes. *)
  type t

  (** Get the severity of a code. *)
  val severity : t -> Severity.t

  (** Get the code number for a code. *)
  val code_num : t -> int

  (** Get a long-form description of a code. *)
  val description : t -> string
end

