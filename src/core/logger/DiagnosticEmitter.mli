module type S = DiagnosticEmitterSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code
