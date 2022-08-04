module type Handler = LoggerSigs.Handler
module type S = LoggerSigs.S

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D =
struct
  module type Handler = Handler with module Code := C and module Diagnostic := D

  module DE = DiagnosticEmitter.Make(C)(D)
  module DB = DiagnosticBuilder.Make(C)(D)

  let messagef = DB.messagef
  let kmessagef = DB.kmessagef
  let tracef = DB.tracef
  let append_marks = DB.append_marks
  let emitf ?loc ?additional_marks ~code = DB.kmessagef DE.Perform.emit ?loc ?additional_marks ~code
  let fatalf ?loc ?additional_marks ~code = DB.kmessagef DE.Perform.fatal ?loc ?additional_marks ~code

  module Run (H : Handler) =
  struct
    module DERun = DE.Run (H)
    let run f = DB.run @@ fun () -> DERun.run f
  end

  module TryWith = DE.TryWith
  module Perform = DE.Perform
end
