module type S = DiagnosticBuilderSigs.S

module Make (Code : Code.S) : S with module Code := Code
