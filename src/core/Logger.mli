module type S = LoggerSigs.S

module Make (Code : Code.S) (Phase : Phase.S) : S with module Code := Code and module Phase := Phase
