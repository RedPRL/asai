module type Handler = LoggerSigs.Handler
module type S = LoggerSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  module type Handler = Handler with module Code := Code

  module DE = DiagnosticEmitter.Make(Code)
  module DB = DiagnosticBuilder.Make(Code)

  let messagef = DB.messagef
  let kmessagef = DB.kmessagef
  let tracef = DB.tracef
  let append_marks = DB.append_marks
  let emitf ?loc ?additional_marks ?severity ~code = DB.kmessagef DE.Perform.emit ?loc ?additional_marks ?severity ~code
  let fatalf ?loc ?additional_marks ?severity ~code = DB.kmessagef DE.Perform.fatal ?loc ?additional_marks ?severity ~code

  module Run (H : Handler) =
  struct
    module DERun = DE.Run (H)
    let run f = DB.run @@ fun () -> DERun.run f
  end

  module TryWith = DE.TryWith
  module Perform = DE.Perform
end
