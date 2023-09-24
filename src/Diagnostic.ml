include DiagnosticData

let text s fmt =
  List.iteri
    (fun i s ->
       if i > 0 then Format.pp_force_newline fmt ();
       Format.pp_print_string fmt s
    ) @@
  String.split_on_char '\n' s

let textf = Format.dprintf

let ktextf = Format.kdprintf

let message ?loc s = Span.{ loc; value = text s }

let kmessagef ?loc k = ktextf @@ fun message -> k Span.{ loc; value = message }

let messagef ?loc = kmessagef Fun.id ?loc

let of_message ?(backtrace=Bwd.Emp) ?(additional_messages=[]) severity code message : _ t =
  { severity
  ; code
  ; message
  ; backtrace
  ; additional_messages
  }

let make ?loc ?backtrace ?additional_messages severity code str =
  of_message ?backtrace ?additional_messages severity code @@ message ?loc str

let kmakef ?loc ?backtrace ?additional_messages k severity code =
  kmessagef ?loc @@ fun message ->
  k @@ of_message ?backtrace ?additional_messages severity code message

let makef ?loc ?backtrace ?additional_messages severity code =
  kmessagef ?loc @@ of_message ?backtrace ?additional_messages severity code

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
