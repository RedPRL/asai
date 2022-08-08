include module type of Marked

module Make (C : Asai.Code.S) :
sig
  val format : splitting_threshold:int -> C.t Asai.Diagnostic.t -> t
end
