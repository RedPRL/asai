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

type t =
  | Range of position * position
  | End_of_file of position

type 'a located = { loc : t option; value : 'a }

let dump_string_source fmt {title; content} =
  Format.fprintf fmt "@[<1>{@[<2>title=@,%a@];@ @[<2>content=@,%a@]}@]"
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

let dump fmt =
  function
  | Range (begin_, ending_) ->
    Format.fprintf fmt {|@[<2>Range@ %a@]|}
      (Utils.dump_pair dump_position dump_position) (begin_, ending_)
  | End_of_file pos ->
    Format.fprintf fmt {|@[<2>End_of_file@ %a@]|}
      dump_position pos

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
    Range (begin_, end_)

let eof pos = End_of_file pos

let view =
  function
  | Range (p1, p2) -> `Range (p1, p2)
  | End_of_file p -> `End_of_file p

let split =
  function
  | Range (p1, p2) -> p1, p2
  | End_of_file _ -> invalid_arg "Asai.Range.split"

let source = function Range (x, _) | End_of_file x -> x.source
let begin_line_num = function Range (x, _) | End_of_file x -> x.line_num
let begin_offset = function Range (x, _) | End_of_file x -> x.offset
let end_line_num = function Range (_, x) | End_of_file x -> x.line_num
let end_offset = function Range (_, x) | End_of_file x -> x.offset

let located loc value = {loc = Some loc; value}
let locate = located
let located_opt loc value = {loc; value}
let locate_opt = located_opt

let of_lex_position ?source (pos : Lexing.position) : position =
  let source = Option.value ~default:(`File pos.pos_fname) source in
  {
    source;
    offset = pos.pos_cnum;
    start_of_line = pos.pos_bol;
    line_num = pos.pos_lnum;
  }

let of_lex_range ?source (begin_, end_) =
  make (of_lex_position ?source begin_, of_lex_position ?source end_)

let of_lexbuf ?source lexbuf =
  of_lex_range ?source (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let located_lex ?source r v = located (of_lex_range ?source r) v
let locate_lex = located_lex
