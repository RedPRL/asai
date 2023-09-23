include DiagnosticData

let ktextf = Format.kdprintf

let text_of_string s fmt =
  List.iteri
    (fun i s ->
       if i > 0 then Format.pp_force_newline fmt ();
       Format.pp_print_string fmt s
    ) @@
  String.split_on_char '\n' s

let textf = Format.dprintf

let kmessagef k ?loc = ktextf @@ fun message -> k Span.{ loc; value = message }

let message_of_string ?loc s =
  Span.{ loc; value = text_of_string s }

let messagef ?loc = kmessagef Fun.id ?loc

let string_of_severity =
  function
  | Hint -> "Hint"
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
  | Bug -> "Bug"

let string_of_text text : string =
  let buf = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer buf in
  let () = Format.pp_set_geometry fmt ~max_indent:(Int.max_int-1) ~margin:Int.max_int in
  text fmt;
  Buffer.contents buf

let map f d = {d with code = f d.code}
