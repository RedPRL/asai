module type Handler = DiagnosticEmitterSigs.Handler
module type S = DiagnosticEmitterSigs.S

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D
