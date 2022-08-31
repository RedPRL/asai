(** Diagnostic display for UNIX terminals. *)
open Asai

(** {1 Display}
    This module provides functions to display and interact with Asai diagnostics in UNIX terminals.
*)
module Make (Code : Code.S) (Phase : Phase.S) : sig
  (** [display trace_phases diag] displays the message provided in [diag],
      along with relevant text from its span, optionally displaying the traces in [diag] filtered by [trace_phases]
  *)
  val display : (Phase.t -> bool) -> (Code.t, Phase.t) Diagnostic.t -> unit

  (** [interactive_trace trace_phases diag] drops the user in a small interactive terminal app where they can cycle through
      the message provided in [diag] and its traces, filtered by [trace_phases]
  *)
  val interactive_trace : (Phase.t -> bool) -> (Code.t, Phase.t) Diagnostic.t -> unit
end
