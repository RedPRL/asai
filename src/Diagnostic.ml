open Bwd

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

let loctext ?loc s = Span.{ loc; value = text s }

let kloctextf ?loc k = ktextf @@ fun loctext -> k Span.{ loc; value = loctext }

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
  Format.pp_print_flush fmt ();
  Buffer.contents buf
