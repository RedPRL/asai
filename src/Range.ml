type string_source = {
  title: string option;
  content: string;
}

type source = [`File of string | `String of string_source]

type position = {
  source : source;
  offset : int;
  start_of_line : int;
  line_num : int;
}

type t = position * position

type 'a located = { loc : t option; value : 'a }

let dump_string_source fmt {title; content} =
  Format.fprintf fmt "@[{title=%a;@ content=%a}@]"
    (Utils.dump_option Utils.dump_string) title Utils.dump_string content

let dump_source fmt : source -> unit =
  function
  | `File s -> Format.fprintf fmt "(`File %a)" Utils.dump_string s
  | `String str_src ->
    Format.fprintf fmt "@[<3>(`String@ @[%a@])@]"
      dump_string_source str_src

let dump_position fmt {source; offset; start_of_line; line_num} =
  Format.fprintf fmt {|@[<1>{@[<2>source=@[%a@]@];@ offset=%d;@ start_of_line=%d;@ line_num=%d}@]|}
    dump_source source offset start_of_line line_num

let dump = Utils.dump_pair dump_position dump_position

let title : source -> string option =
  function
  | `String {title; _} -> title
  | `File p -> Some p

let make (begin_ , end_ : position * position) : t =
  if begin_.source <> end_.source then
    invalid_arg @@
    Format.asprintf "make: source %a and %a do not match"
      dump_source begin_.source
      dump_source end_.source
  else if begin_.offset > end_.offset || begin_.line_num > end_.line_num || begin_.start_of_line > end_.start_of_line then
    invalid_arg @@
    Format.asprintf "make: the ending position comes before the starting position"
  else
    begin_, end_

let split (sp : t) : position * position = sp

let source ((begin_,_) : t) = begin_.source
let begin_line_num ((begin_, _) : t) = begin_.line_num
let begin_offset ((begin_, _) : t) = begin_.offset
let end_line_num ((_, end_) : t) = end_.line_num
let end_offset ((_, end_) : t) = end_.offset

let locate_opt loc value = {loc; value}
let locate loc value = {loc = Some loc; value}

let of_lex_position (pos : Lexing.position) : position =
  {
    source = `File pos.pos_fname;
    offset = pos.pos_cnum;
    start_of_line = pos.pos_bol;
    line_num = pos.pos_lnum;
  }

let of_lex_span (start, stop) =
  make (of_lex_position start, of_lex_position stop)

let of_lexbuf lexbuf =
  of_lex_span (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let locate_lex sp v = locate (of_lex_span sp) v
