type position = {
  file_path : string;
  offset : int;
  start_of_line : int;
  line_num : int;
}

type t = {
  file_path : string;
  begin_offset : int;
  begin_start_of_line : int;
  begin_line_num : int;
  end_offset : int;
  end_start_of_line : int;
  end_line_num : int;
}

type 'a located = { loc : t option; value : 'a }

let make (begin_ , end_ : position * position) : t =
  if begin_.file_path <> end_.file_path then
    invalid_arg @@
    Format.asprintf "make: filenames %S and %S do not match"
      begin_.file_path
      end_.file_path
  else if begin_.offset > end_.offset || begin_.line_num > end_.line_num || begin_.start_of_line > end_.start_of_line then
    invalid_arg @@
    Format.asprintf "make: the ending position comes before the starting position"
  else {
    file_path = begin_.file_path;
    begin_offset = begin_.offset;
    begin_start_of_line = begin_.start_of_line;
    begin_line_num = begin_.line_num;
    end_offset = end_.offset;
    end_start_of_line = end_.start_of_line;
    end_line_num = end_.line_num;
  }

let split (sp : t) : position * position =
  {
    file_path = sp.file_path;
    offset = sp.begin_offset;
    start_of_line = sp.begin_start_of_line;
    line_num = sp.begin_line_num;
  }, {
    file_path = sp.file_path;
    offset = sp.end_offset;
    start_of_line = sp.end_start_of_line;
    line_num = sp.end_line_num;
  }

let file_path sp = sp.file_path
let begin_line_num sp = sp.begin_line_num
let end_line_num sp = sp.end_line_num
let begin_offset sp = sp.begin_offset
let end_offset sp = sp.end_offset

let locate loc value = {loc; value}

let of_lex_position (pos : Lexing.position) : position =
  {
    file_path = pos.pos_fname;
    offset = pos.pos_cnum;
    start_of_line = pos.pos_bol;
    line_num = pos.pos_lnum;
  }

let of_lex_span (start, stop) =
  make (of_lex_position start, of_lex_position stop)

let of_lexbuf lexbuf =
  of_lex_span (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let locate_lex sp v = locate (Some (of_lex_span sp)) v
