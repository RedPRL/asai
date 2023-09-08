(** A LSP server for asai *)

[@@@alert unstable
    "The LSP backend may change in significant ways in the future."
]

open Asai

(** {1 LSP}
    A large part of the LSP protocol is about handling location-related information, which
    makes it possible to provide a generic LSP server for any tool using asai. *)

module Make (Code : Diagnostic.Code) : sig
  val run : init:(string option -> unit)
    -> load_file:(string -> unit)
    -> inner_run:
         (?init_backtrace:Asai.Diagnostic.message Asai.Span.located Bwd.bwd
          -> emit:(Code.t Asai.Diagnostic.t -> unit)
          -> fatal:(Code.t Asai.Diagnostic.t -> unit)
          -> (unit -> unit) -> unit)
    -> unit
end
