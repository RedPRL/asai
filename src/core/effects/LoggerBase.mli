module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  module type Handler = 
  sig
    type result
    val print : Diagnostic.t -> unit
    val fatal : Diagnostic.t -> result
  end

  module Run (H : Handler) :
  sig
    val run : (unit -> H.result) -> H.result
  end

  module TryWith (H : Handler) :
  sig
    val try_with : (unit -> H.result) -> H.result
  end

  module Perform :
  sig
    val print : Diagnostic.t -> unit
    val fatal : Diagnostic.t -> 'a
  end
end


module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D
