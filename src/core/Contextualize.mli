module Make : Reader.S ->
  sig
    val contextualize : splitting_threshold:int -> 'code Diagnostic.t -> 'code Context.diagnostic
  end
