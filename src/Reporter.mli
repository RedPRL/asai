(** The signature of a logger. *)
module type S = ReporterSigs.S

(** The functor to generate a logger. *)
module Make (Code : Diagnostic.Code) : S with module Code := Code
