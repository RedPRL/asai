module type S = LoggerSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  module DE = DiagnosticEmitter.Make(Code)
  module DB = DiagnosticBuilder.Make(Code)

  let messagef = DB.messagef
  let kmessagef = DB.kmessagef
  let tracef = DB.tracef
  let append_marks = DB.append_marks
  let emit = DE.emit
  let emitf ?loc ?additional_marks ?severity ~code = DB.kmessagef emit ?loc ?additional_marks ?severity ~code
  let fatal = DE.fatal
  let fatalf ?loc ?additional_marks ?severity ~code = DB.kmessagef fatal ?loc ?additional_marks ?severity ~code
  let run ~emit ~fatal f = DB.run @@ fun () -> DE.run ~emit ~fatal f
  let try_with = DE.try_with
end
