module type S =
sig
  (** [assemble ~splitting_threshold d] reads the file content and flatten the spans into marked text. *)
  val assemble : splitting_threshold:int -> ('code,'phase) Asai.Diagnostic.t -> ('code,'phase) Marked.t
end

module Make (R : Reader.S) : S
