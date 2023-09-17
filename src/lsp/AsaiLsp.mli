(** @canonical Asai.Lsp *)

[@@@alert unstable
    "The LSP backend may change significantly in the future."
]

(** {1 Language Service Protocol} *)

(** This module provides a rudimentary implementation of the LSP protocol.

    Note: this interface is experimental, and would likely change in the future.

    @canonical Asai.Lsp.Make *)
module Make (Code : Diagnostic.Code) : sig
  val start : source:string option
    -> init:(root:string option -> unit)
    -> load_file:(display:(Code.t Diagnostic.t -> unit) -> string -> unit)
    -> unit
    (** [run ~init ~load_file] starts the LSP server with the two callbacks [init] and [load_file].

        @param source The source of a LSP diagnostic, that is, a "human-readable string describing the source of this diagnostic."
        @param init The callback to initiate the loading of a workspace. The [root] parameter is the workspace
        @param load_file The callback to load the file.
    *)
end
