(** The signature of a reporter.

    @inline *)
include module type of ReporterSigs

(** The functor to generate a reporter. *)
module Make (Message : Message) : S with module Message := Message
