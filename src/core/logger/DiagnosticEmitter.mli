module type Handler = DiagnosticEmitterSigs.Handler
module type S = DiagnosticEmitterSigs.S

module Make (Code : Code.S) : S with module Code := Code
