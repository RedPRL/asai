module type Handler = LoggerSigs.Handler
module type S = LoggerSigs.S

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D
