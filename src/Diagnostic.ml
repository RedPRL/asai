open Bwd

include DiagnosticData

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
  of_text ?loc ?backtrace ?extra_remarks severity message @@ Text.make explanation

let kmakef ?loc ?backtrace ?extra_remarks k severity message =
  Text.kmakef @@ fun text ->
  k @@ of_text ?loc ?backtrace ?extra_remarks severity message text

let makef ?loc ?backtrace ?extra_remarks severity message =
  Text.kmakef @@ of_text ?loc ?backtrace ?extra_remarks severity message

let map f d = {d with message = f d.message}

let string_of_text text : string =
  let buf = Buffer.create 20 in
  let fmt = Format.formatter_of_buffer buf in
  let () = Format.pp_set_geometry fmt ~max_indent:2 ~margin:(Format.pp_infinity-1) in
  text fmt;
  Format.pp_print_flush fmt ();
  Str.global_replace (Str.regexp "\\([\r\n]+ *\\)+") " " @@
  Buffer.contents buf

type text = Text.t
let text = Text.make
let textf = Text.makef
let ktextf = Text.kmakef

type loctext = Loctext.t
let loctext = Loctext.make
let loctextf = Loctext.makef
let kloctextf = Loctext.kmakef
