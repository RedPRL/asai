include StructuredReporterSigs

module Make (Message : Message) : S with module Message := Message =
struct
  include Reporter.Make(Message)

  let get_severity code = function None -> Message.default_severity code | Some severity -> severity
  let get_text code = function None -> Message.default_text code | Some text -> text
  let get_merged_loc = function None -> get_loc() | loc -> loc

  let diagnostic ?severity ?loc ?text ?(backtrace=get_backtrace()) ?extra_remarks msg =
    Diagnostic.of_text ?loc:(get_merged_loc loc) ~backtrace ?extra_remarks (get_severity msg severity) msg (get_text msg text)

  let emit ?severity ?loc ?text ?backtrace ?extra_remarks msg =
    emit_diagnostic @@ diagnostic ?severity ?loc ?text ?backtrace ?extra_remarks msg

  let fatal ?severity ?loc ?text ?backtrace ?extra_remarks msg =
    fatal_diagnostic @@ diagnostic ?severity ?loc ?text ?backtrace ?extra_remarks msg
end
