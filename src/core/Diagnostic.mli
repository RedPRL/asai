type message = DiagnosticSigs.message
module type S = DiagnosticSigs.S

(** The functor to generate a diagnostic module from an error code module. *)
module Make (C : Code.S) : S with module Code := C
