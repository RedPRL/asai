include DiagnosticData

let string_of_severity =
  function
  | Hint -> "Hint"
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
  | Bug -> "Bug"

let string_of_message msg : string =
  let buf = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer buf in
  let () = Format.pp_set_geometry fmt ~max_indent:(Int.max_int-1) ~margin:Int.max_int in
  msg fmt;
  Buffer.contents buf

let map f d = {d with code = f d.code}
