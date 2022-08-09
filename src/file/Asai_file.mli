(** Read files and flatten spans into marked text.

    {[
      module F = Asai_file.Make (Code) (Diagnostic)
      let text = F.format ~splitting_threshold:5 diagnostic
    ]}

*)

module Marked = Marked

(** [format ~splitting_threshold d] reads the file content and flatten the spans into marked text. *)
val format : splitting_threshold:int -> 'code Asai.Diagnostic.t -> 'code Marked.t

(**/**)

module Internal :
sig
  module Reader = Reader
  module Flattened = Flattened
  module Flattener = Flattener
  module Marker = Marker
end
