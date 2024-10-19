(** Diagnostic display for GitHub Actions. *)

module Make (Message : Reporter.Message) = struct
  let command_of_severity =
    function
    | Diagnostic.Info -> "notice"
    | Diagnostic.Hint -> "notice"
    | Diagnostic.Warning -> "warning"
    | Diagnostic.Error -> "error"
    | Diagnostic.Bug -> "error"

  let print_without_loc severity msg text =
    Format.printf "::%s title=%s::%s@."
      (command_of_severity severity)
      (Message.short_code msg)
      (Text.to_string text)

  let print_with_loc severity msg loc text =
    match Range.source loc with
    | `String _ ->
      print_without_loc severity msg text
    | `File file_path ->
      Format.printf "::%s file=%s,line=%i,endLine=%i,title=%s::%s@."
        (command_of_severity severity)
        file_path
        (Range.begin_line_num loc)
        (Range.end_line_num loc)
        (Message.short_code msg)
        (Text.to_string text)

  let print Diagnostic.{severity; message; explanation = {loc; value = text}; _} =
    match loc with
    | Some loc -> print_with_loc severity message loc text
    | None -> print_without_loc severity message text
end
