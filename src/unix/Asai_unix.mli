(** Diagnostic display for UNIX terminals. *)
open Asai

(** {1 Display}
    This module provides a single function for display Asai diagnostics to UNIX terminals. *)
module Make (Code : Code.S) : sig
    val display : ?display_traces:bool -> Code.t Diagnostic.t -> unit
end
