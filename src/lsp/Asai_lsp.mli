(** A LSP server for asai *)

[@@@alert unstable
    "The LSP backend may change in significant ways in the future."
]

open Asai

(** {1 LSP}

    This module provides a rudimentary implementation of the LSP protocol.


    A large part of the LSP protocol is about handling location-related information, which
    makes it possible to provide a generic LSP server for any tool using asai.


*)

module Make (Code : Diagnostic.Code) : sig
  val run : init:(string option -> unit)
    -> load_file:((Code.t Asai.Diagnostic.t -> unit) -> string -> unit)
    -> unit
end
