(** The signature of a logger. *)
module type S = LoggerSigs.S

(** The functor to generate a logger. *)
module Make (Code : Diagnostic.Code) : S with module Code := Code
