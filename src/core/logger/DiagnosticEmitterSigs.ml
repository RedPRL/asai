module type S =
sig
  module Code : Code.S
  module Phase : Phase.S

  (** Emit a diagnostic and continue the computation. *)
  val emit : (Code.t, Phase.t) Diagnostic.t -> unit

  (** Emit a diagnostic and abort the computation. *)
  val fatal: (Code.t, Phase.t) Diagnostic.t -> 'a

  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle emitted diagnostics before continuing
      the computation, and [fatal] to handle diagnostics after aborting the computation. *)
  val run : emit:((Code.t, Phase.t) Diagnostic.t -> unit) -> fatal:((Code.t, Phase.t) Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  (** [try_with ~emit ~fatal f] runs the thunk [f], using [emit] to intercept emitted diagnostics before continuing
      the computation, and [fatal] to intercept diagnostics after aborting the computation. *)
  val try_with : ?emit:((Code.t, Phase.t) Diagnostic.t -> unit) -> ?fatal:((Code.t, Phase.t) Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
end
