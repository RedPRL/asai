type id = Severity.t * int

module type S =
sig
  (** The type of message codes. *)
  type t

  (** The type of unique IDs, such as [(Error, 100)] which represents "Error #100". *)
  type nonrec id = id

  (** Get the unique ID *)
  val id : t -> id

  (** Get a long-form description of a code. *)
  val description : t -> string
end
