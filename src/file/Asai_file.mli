(** Read files and flatten spans into marked text. *)

(** Definition of marked text. *)
module Marked = Marked

(** [assemble ~splitting_threshold d] reads the file content and flatten the spans into marked text. *)
val assemble : splitting_threshold:int -> 'code Asai.Diagnostic.t -> 'code Marked.t

(**/**)

module Internal :
sig
  module Reader = Reader
  module Flattened = Flattened
  module Flattener = Flattener
  module Marker = Marker
end
