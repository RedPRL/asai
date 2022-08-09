module type Handler =
sig
  module Code : Code.S

  (** The handler for the algebraic effects to emit diagnostics. *)
  val emit : Code.t Diagnostic.t -> unit

  (** The result type for the exception handler {!val:fatal}. *)
  type result

  (** The handler for the exceptions that carry diagnostics. *)
  val fatal : Code.t Diagnostic.t -> result
end

module type S =
sig
  module Code : Code.S

  module type Handler = Handler with module Code := Code

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
    val emit : Code.t Diagnostic.t -> unit
    val fatal : Code.t Diagnostic.t -> 'a
  end
end
