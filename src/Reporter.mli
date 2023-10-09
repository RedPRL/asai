(** The signature of a logger.

    @inline *)
include module type of ReporterSigs

(** The functor to generate a logger. *)
module Make (Code : Code) : S with module Code := Code
