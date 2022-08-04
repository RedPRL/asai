module type Handler =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  (** The handler for the algebraic effects to emit diagnostics. *)
  val emit : Diagnostic.t -> unit

  (** The result type for the exception handler {!val:fatal}. *)
  type result

  (** The handler for the exceptions that carry diagnostics. *)
  val fatal : Diagnostic.t -> result
end

module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  module type Handler = Handler with module Code := Code and module Diagnostic := Diagnostic

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
    val emit : Diagnostic.t -> unit
    val fatal : Diagnostic.t -> 'a
  end
end
