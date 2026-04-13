open Bwd

include Diagnostic_data

let make ?(backtrace=Bwd.Emp) ?(extra_remarks=[]) severity message explanation : _ t =
  { severity
  ; message
  ; explanation
  ; backtrace
  ; extra_remarks = Bwd.of_list extra_remarks
  }

let map f1 f2 d = {d with message = f1 d.message; explanation = f2 d.explanation}

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
