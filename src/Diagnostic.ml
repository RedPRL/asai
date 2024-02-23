open Bwd

include DiagnosticData

let text s fmt =
  Format.(pp_print_list ~pp_sep:pp_force_newline pp_print_string) fmt @@
  String.split_on_char '\n' s

let textf = Format.dprintf

let ktextf = Format.kdprintf

let loctext ?loc s = Range.{ loc; value = text s }

let kloctextf ?loc k = ktextf @@ fun loctext -> k Range.{ loc; value = loctext }

let loctextf ?loc = kloctextf Fun.id ?loc

let of_loctext ?(backtrace=Bwd.Emp) ?(extra_remarks=[]) severity message explanation : _ t =
  { severity
  ; message
  ; explanation
  ; backtrace
  ; extra_remarks = Bwd.of_list extra_remarks
  }

let of_text ?loc ?backtrace ?extra_remarks severity message text : _ t =
  of_loctext ?backtrace ?extra_remarks severity message {loc; value = text}

let make ?loc ?backtrace ?extra_remarks severity message explanation =
  of_text ?loc ?backtrace ?extra_remarks severity message @@ text explanation

let kmakef ?loc ?backtrace ?extra_remarks k severity message =
  ktextf @@ fun text ->
  k @@ of_text ?loc ?backtrace ?extra_remarks severity message text

let makef ?loc ?backtrace ?extra_remarks severity message =
  ktextf @@ of_text ?loc ?backtrace ?extra_remarks severity message

let map f d = {d with message = f d.message}

let string_of_text text : string =
  let buf = Buffer.create 20 in
  let fmt = Format.formatter_of_buffer buf in
  let () = Format.pp_set_geometry fmt ~max_indent:2 ~margin:Format.pp_infinity in
  text fmt;
  Format.pp_print_flush fmt ();
  Str.global_replace (Str.regexp "\\([\r\n]+ *\\)+") " " @@
  Buffer.contents buf
