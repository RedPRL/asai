include module type of Explicated

module type Reader = Reader.S

module Make : Reader ->
  sig
    val contextualize : splitting_threshold:int -> 'code Diagnostic.t -> 'code Explicated.diagnostic
  end
