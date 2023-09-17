include DiagnosticData

let ktextf = Format.kdprintf

let textf = Format.dprintf

let kmessagef k ?loc = ktextf @@ fun message -> k Span.{ loc; value = message }

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
