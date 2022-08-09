module R = Reader

type position = Asai.Span.position

let find_eol (pos : Asai.Span.position) =
  let eof = R.length pos.file_path in
  let rec go i =
    if i < eof && R.unsafe_get pos.file_path i = '\n' then
      i
    else
      go (i+1)
  in
  {pos with offset = go pos.offset}

let eol_to_next_line (pos : position) =
  { pos with
    start_of_line = pos.offset + 1;
    line_num = pos.line_num + 1 }

let read_between (begin_ : position) (end_ : position) =
  String.init (end_.offset - begin_.offset) @@ fun i ->
  R.unsafe_get begin_.file_path (begin_.offset + i)

open Bwd
open BwdNotation

let append_segment segments style begin_ end_ =
  match read_between begin_ end_ with
  | "" -> segments
  | s -> segments #< (style, s)

let mark_block (b : Flattened.block) : Marked.block =
  if b = [] then invalid_arg "mark_block"
  else
    let rec go lines segments style (cursor : position) : Flattened.block -> _ =
      function
      | [] ->
        let segments = append_segment segments style cursor (find_eol cursor) in
        lines #< (Bwd.to_list segments)
      | p::ps ->
        if cursor.line_num = (snd p).line_num then
          let segments = append_segment segments style cursor (snd p) in
          go lines segments (fst p) (snd p) ps
        else
          let eol = find_eol cursor in
          let segments = append_segment segments style cursor eol in
          go (lines #< (Bwd.to_list segments)) Emp style (eol_to_next_line eol) (p :: ps)
    in
    let start_pos = Asai.Span.to_start_of_line @@ snd @@ List.hd b in
    { start_line_num = start_pos.line_num
    ; lines = Bwd.to_list @@ go Emp Emp None start_pos b
    }

let mark_blocks = List.map mark_block
let mark_section (file_path, bs) : Marked.section =
  { file_path; blocks = mark_blocks bs }
