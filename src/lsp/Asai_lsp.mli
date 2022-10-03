(** A LSP server for asai *)

open Asai

(** {1 LSP}
    A large part of the LSP protocol is about handling location-related information, which
    makes it possible to provide a generic LSP server for any tool using asai. *)

module Make (ErrorCode: ErrorCode.S) : sig
  module Doctor : module type of Asai.Effects.Make(ErrorCode)

  val run : init:(string option -> unit)
    -> load_file:(string -> unit)
    -> unit
end
