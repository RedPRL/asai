type t = Format.formatter -> unit

let make s fmt =
  Format.(pp_print_list ~pp_sep:pp_force_newline pp_print_string) fmt @@
  String.split_on_char '\n' s

let makef = Format.dprintf

let kmakef = Format.kdprintf

let to_string text : string =
  let buf = Buffer.create 20 in
  let fmt = Format.formatter_of_buffer buf in
  let () = Format.pp_set_geometry fmt ~max_indent:2 ~margin:(Format.pp_infinity-1) in
  text fmt;
  Format.pp_print_flush fmt ();
  Str.global_replace (Str.regexp "\\([\r\n]+ *\\)+") " " @@
  Buffer.contents buf
