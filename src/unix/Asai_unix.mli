(** Diagnostic display for UNIX terminals. *)
open Asai

(** This module provides a single function for display Asai diagnostics to UNIX terminals. *)

val display : 'code Diagnostic.t -> unit
