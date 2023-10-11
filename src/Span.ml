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

let dump_string fmt s = Format.fprintf fmt "%S" s [@@inline]

let dump_option dump fmt =
  function
  | None -> Format.pp_print_string fmt "None"
  | Some v -> Format.fprintf fmt "Some %a" dump v

let dump_string_source fmt {title; content} =
  Format.fprintf fmt "@[{title=%a;@ content=%a}@]"
    (dump_option dump_string) title dump_string content

let dump_source fmt : source -> unit =
  function
  | `File s -> Format.fprintf fmt "(`File %a)" dump_string s
  | `String str_src ->
    Format.fprintf fmt "@[<3>(`String@ @[%a@])@]"
      dump_string_source str_src

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

let source (beginning,_) = beginning.source
let begin_line_num (beginning, _) = beginning.line_num
let end_line_num (_,ending) = ending.line_num
let begin_offset (beginning,_) = beginning.offset
let end_offset (_,ending) = ending.offset

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
