module type S =
sig
  (** An abstract type of error codes. *)
  type t

  (** Get the severity of an error code. *)
  val severity : t -> Severity.t

  (** Get the code number for an error code. *)
  val code_num : t -> int

  (** Get a long-form description of an error code. *)
  val description : t -> string
end

