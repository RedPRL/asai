(** Diagnostic display for GitHub Actions. *)

module Make (Code : Reporter.Code) = struct
  let command_of_severity =
    function
    | Diagnostic.Info -> "notice"
    | Diagnostic.Hint -> "notice"
    | Diagnostic.Warning -> "warning"
    | Diagnostic.Error -> "error"
    | Diagnostic.Bug -> "error"

  let single_line_of_text msg =
    String.map
      (function
        | '\r' | '\n' -> ' '
        | c -> c) @@
    Diagnostic.string_of_text msg

  let print_with_loc severity code loc msg =
    match Span.source loc with
    | `String _ ->
      Format.printf "::%s title=%s::%s@."
        (command_of_severity severity)
        (Code.to_string code)
        (single_line_of_text msg)
    | `File file_path ->
      Format.printf "::%s file=%s,line=%i,endLine=%i,title=%s::%s@."
        (command_of_severity severity)
        file_path
        (Span.begin_line_num loc)
        (Span.end_line_num loc)
        (Code.to_string code)
        (single_line_of_text msg)

  let print_without_loc severity code msg =
    Format.printf "::%s title=%s::%s@."
      (command_of_severity severity)
      (Code.to_string code)
      (single_line_of_text msg)

  let print Diagnostic.{severity; code; message = {loc; value = msg};_} =
    match loc with
    | Some loc -> print_with_loc severity code loc msg
    | None -> print_without_loc severity code msg
end
