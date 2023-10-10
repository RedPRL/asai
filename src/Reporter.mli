(** The signature of a reporter.

    @inline *)
include module type of ReporterSigs

(** The functor to generate a logger. *)
module Make (Message : Message) : S with module Message := Message
