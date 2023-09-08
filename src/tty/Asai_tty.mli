(** Diagnostic display for UNIX terminals. *)
open Asai

(** {1 Display}
    This module provides functions to display or interact with diagnostics in UNIX terminals.

    [@@@alert unstable
      "The TTY backend will likely change in significant ways in the future to account for more features."
    ]

    @unstable
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
