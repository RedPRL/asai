(** Diagnostic display for UNIX terminals. *)

[@@@alert unstable
    "The TTY backend will likely change significantly in the future to account for more features."
]

(** {1 Display} *)

(** This module provides functions to display or interact with diagnostics in UNIX terminals. *)
module Make (Code : Asai.Diagnostic.Code) : sig

  (** [display d] prints the diagnostic [d] to the standard output, using terminal control characters for formatting.

      @param backtrace Whether the backtrace should be shown. The default is [false].
  *)
  val display : ?backtrace:bool -> Code.t Asai.Diagnostic.t -> unit

  (** [interactive_trace d] drops the user in a small interactive terminal app where they can cycle through the message provided in [d] and its backtrace. *)
  val interactive_trace : Code.t Asai.Diagnostic.t -> unit
end
