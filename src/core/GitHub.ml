(** Diagnostic display for GitHub Actions. *)

module Make (Code : Diagnostic.Code) = struct
  let command_of_severity =
    function
    | Diagnostic.Info -> "notice"
    | Diagnostic.Hint -> "notice"
    | Diagnostic.Warning -> "warning"
    | Diagnostic.Error -> "error"
    | Diagnostic.Bug -> "error"

  let print_with_loc severity code loc msg =
    Format.printf "::%s file=%s,line=%i,endLine=%i,title=%s::%s@."
      (command_of_severity severity)
      (Span.file_path loc)
      (Span.begin_line_num loc)
      (Span.end_line_num loc)
      (Code.to_string code)
      (Diagnostic.string_of_message msg)

  let print_without_loc severity code msg =
    Format.printf "::%s title=%s::%s@."
      (command_of_severity severity)
      (Code.to_string code)
      (Diagnostic.string_of_message msg)

  let print Diagnostic.{severity; code; message = {loc; value = msg};_} =
    match loc with
    | Some loc -> print_with_loc severity code loc msg
    | None -> print_without_loc severity code msg
end
