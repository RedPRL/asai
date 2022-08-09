module type Handler = LoggerSigs.Handler
module type S = LoggerSigs.S

module Make (Code : Code.S) : S with module Code := Code
