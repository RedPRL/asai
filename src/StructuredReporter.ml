include StructuredReporterSigs

module Make (Message : Message) : S with module Message := Message =
struct
  module R = Reporter.Make(Message)

  let get_loc = R.get_loc
  let with_loc = R.with_loc
  let merge_loc = R.merge_loc
  let get_backtrace = R.get_backtrace
  let with_backtrace = R.with_backtrace
  let trace_text = R.trace_text
  let trace_loctext = R.trace_loctext
  let trace = R.trace
  let tracef = R.tracef

  let get_severity code = function None -> Message.default_severity code | Some severity -> severity
  let get_text code = function None -> Message.default_text code | Some text -> text
  let get_merged_loc = function None -> get_loc() | loc -> loc

  let diagnostic ?severity ?loc ?text ?(backtrace=get_backtrace()) ?extra_remarks msg =
    Diagnostic.of_text ?loc:(get_merged_loc loc) ~backtrace ?extra_remarks (get_severity msg severity) msg (get_text msg text)

  (* Sending messages *)

  let emit_diagnostic = R.emit_diagnostic
  let fatal_diagnostic = R.fatal_diagnostic

  (* Algebraic effects *)

  let run = R.run
  let try_with = R.try_with

  (* Convenience functions *)

  let emit ?severity ?loc ?text ?backtrace ?extra_remarks msg =
    emit_diagnostic @@ diagnostic ?severity ?loc ?text ?backtrace ?extra_remarks msg

  let fatal ?severity ?loc ?text ?backtrace ?extra_remarks msg =
    fatal_diagnostic @@ diagnostic ?severity ?loc ?text ?backtrace ?extra_remarks msg

  let adopt = R.adopt
  let map_diagnostic = R.map_diagnostic
  let map_text = R.map_text

  (* Debugging *)

  let register_printer = R.register_printer
end
