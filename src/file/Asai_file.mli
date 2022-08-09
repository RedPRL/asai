include module type of Marked

module type S =
sig
  module Code : Asai.Code.S

  (** [format ~splitting_threshold d] reads the file content and flatten the spans into marked text that can be further processed without consulting the {!module:Code} or {!module:Diagnostic} module. *)
  val format : splitting_threshold:int -> Code.t Asai.Diagnostic.t -> t
end

module Make (Code : Asai.Code.S) : S with module Code := Code

(**/**)

module Internal :
sig
  module Reader : module type of Reader
  module Flattened : module type of Flattened
  module Flatter : module type of Flatter
  module Marker : module type of Marker
end
