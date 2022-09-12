module type S = LoggerSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  module DE = DiagnosticEmitter.Make(Code)
  module DB = DiagnosticBuilder.Make(Code)

  let backtrace = DB.backtrace
  let messagef = DB.messagef
  let kmessagef = DB.kmessagef
  let tracef = DB.tracef
  let ktracef = DB.ktracef
  let append_marks = DB.append_marks
  let emit = DE.emit
  let emitf ?loc ?additional_marks ?severity code = DB.kmessagef emit ?loc ?additional_marks ?severity code
  let fatal = DE.fatal
  let fatalf ?loc ?additional_marks ?severity code = DB.kmessagef fatal ?loc ?additional_marks ?severity code
  let run ?init_backtrace ~emit ~fatal f = DB.run ?init:init_backtrace @@ fun () -> DE.run ~emit ~fatal f
  let bridge m run f = run ?init_backtrace:(Some (backtrace ())) ~emit:(fun d -> emit (m d)) ~fatal:(fun d -> fatal (m d)) f
  let try_with = DE.try_with
end
