module type S = LoggerSigs.S

module Make (Code : Diagnostic.Code) : S with module Code := Code
