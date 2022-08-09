include module type of Marked

module Make (C : Asai.Code.S) :
sig
  val format : splitting_threshold:int -> C.t Asai.Diagnostic.t -> t
end

(**/**)

module Internal :
sig
  module FileReader : module type of FileReader
  module Flattened : module type of Flattened
  module Flatter : module type of Flatter
  module Marker : module type of Marker
end
