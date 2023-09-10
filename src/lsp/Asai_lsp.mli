(** A LSP server for asai *)

[@@@alert unstable
    "The LSP backend may change in significant ways in the future."
]

open Asai

(** {1 LSP}

    This module provides a rudimentary implementation of the LSP protocol.

    Note: this interface is experimental, and would likely change in the future. *)
module Make (Code : Diagnostic.Code) : sig
  val start : init:(root:string option -> unit)
    -> load_file:(display:(Code.t Asai.Diagnostic.t -> unit) -> string -> unit)
    -> unit
    (** [run ~init ~load_file] starts the LSP server with the two callbacks [init] and [load_file].

        @param init The callback to initiate the loading of a workspace. The [root] parameter is the workspace
        @param load_file The callback to load the file.
    *)
end
