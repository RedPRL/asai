(** Diagnostic display for UNIX terminals. *)
open Asai

(** {1 Display}
    This module provides functions to display and interact with asai diagnostics in UNIX terminals.
*)
module Make (Code : Diagnostic.Code) : sig
  (** [display ?display_traces diag] displays the message provided in [diag],
      along with relevant text from its span, optionally displaying the traces in [diag]
  *)
  val display : ?display_traces:bool -> Code.t Diagnostic.t -> unit

  (** [interactive_trace diag] drops the user in a small interactive terminal app where they can cycle through
      the message provided in [diag] and its traces
  *)
  val interactive_trace : Code.t Diagnostic.t -> unit
end
