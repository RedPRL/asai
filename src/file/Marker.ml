module type S =
sig
  val mark_section : Flattened.section -> MarkedDiagnostic.section
end

module Make (R : Reader.S) = struct
  type position = Asai.Span.position

  (** [find_eol pos] finds the position of the next ['\n']. If the end of file is reached before ['\n'], then the position of the end of the file is returned. *)
  let find_eol (pos : position) : position =
    let eof = R.length pos.file_path in
    assert (pos.offset <= eof);
    let[@tailcall] rec go i =
      if i = eof || R.unsafe_get pos.file_path i = '\n' then
        {pos with offset = i}
      else
        go (i+1)
    in
    go pos.offset

  (** Skip the ['\n'] character. *)
  let eol_to_next_line (pos : position) : position =
    { pos with
      (* Need to update our offset to skip the newline char *)
      offset = pos.offset + 1;
      start_of_line = pos.offset + 1;
      line_num = pos.line_num + 1 }

  let read_between (begin_, end_ : position * position) : string =
    String.init (end_.offset - begin_.offset) @@ fun i ->
    R.unsafe_get begin_.file_path (begin_.offset + i)

  let is_empty_span (begin_, end_ : position * position) : bool =
    begin_.offset = end_.offset

  open Bwd
  open Bwd.Infix

  let append_segment (segments : MarkedDiagnostic.segment bwd) (style, span) : MarkedDiagnostic.segment bwd =
    if is_empty_span span
    then segments
    else segments #< (style, read_between span)

  let mark_block : Flattened.block -> MarkedDiagnostic.block =
    function
    | [] -> invalid_arg "mark_block"
    | (b :: _) as bs ->
      let start_pos = Asai.Span.to_start_of_line @@ b.position in
      let[@tailcall] rec go ~lines ~segments (style, cursor : position MarkedDiagnostic.styled)
        : Flattened.block -> MarkedDiagnostic.line bwd =
        function
        | p::ps when cursor.line_num = p.position.line_num ->
          let segments = append_segment segments (style, (cursor, p.position)) in
          go ~lines ~segments (p.style, p.position) ps
        | ps ->
          (* Shift to the next line *)
          let eol = find_eol cursor in
          let segments = append_segment segments (style, (cursor, eol)) in
          let lines = lines #< (Bwd.to_list segments) in
          (** Continue the process if [ps] is not empty. *)
          if ps = [] then lines
          else go ~lines ~segments:Emp (style, eol_to_next_line eol) ps
      in
      { start_line_num = start_pos.line_num
      ; lines = Bwd.to_list @@ go ~lines:Emp ~segments:Emp (None, start_pos) bs
      }

  let mark_blocks = List.map mark_block
  let mark_section (file_path, bs) : MarkedDiagnostic.section =
    { file_path; blocks = mark_blocks bs }
end
