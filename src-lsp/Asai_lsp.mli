(** @canonical Asai.Lsp *)

[@@@alert unstable
    "The LSP implementation and API may change significantly in the future."
]

(** {1 Language Service Protocol} *)

(** This module provides a rudimentary and incomplete implementation of the LSP protocol.

    Note: many features are missing and it does not handle [positionEncoding].

    @canonical Asai.Lsp.Make *)
module Make (Message : Asai.MinimumSigs.Message) : sig
  val start : source:string option
    -> init:(root:string option -> unit)
    -> load_file:(display:(Message.t Asai.Diagnostic.t -> unit) -> string -> unit)
    -> unit
    (** [run ~init ~load_file] starts the LSP server with the two callbacks [init] and [load_file].

        @param source The source of a LSP diagnostic, that is, a "human-readable string describing the source of this diagnostic."
        @param init The callback to initiate the loading of a workspace. The [root] parameter is the workspace
        @param load_file The callback to load the file.
    *)
end
