module type S = DiagnosticBuilderSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code
