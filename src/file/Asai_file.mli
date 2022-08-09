(** Read files and flatten spans into marked text.

    {[
      module F = Asai_file.Make (Code) (Diagnostic)
      let text = F.format ~splitting_threshold:5 diagnostic
    ]}

*)

module Marked = Marked

module type S =
sig
  module Code : Asai.Code.S

  (** [format ~splitting_threshold d] reads the file content and flatten the spans into marked text. *)
  val format : splitting_threshold:int -> Code.t Asai.Diagnostic.t -> Code.t Marked.t
end

module Make (Code : Asai.Code.S) : S with module Code := Code

(**/**)

module Internal :
sig
  module Reader = Reader
  module Flattened = Flattened
  module Flatter = Flatter
  module Marker = Marker
end
