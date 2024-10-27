include SourceUtilsData

exception Invalid_offset of invalid_offset
exception Invalid_position of invalid_position

let find_eol_traditional read (i, eof) =
  let rec go i =
    if i >= eof then
      eof, None
    else
      match read i with
      | '\n' -> i, Some 1 (* LF *)
      | '\r' ->
        if i+1 < eof && read (i+1) = '\n'
        then i, Some 2 (* CRLF *)
        else i, Some 1 (* CR *)
      | _ ->
        (go[@tailcall]) (i+1)
  in
  go i

let find_eol_unicode read (i, eof) =
  let rec go i =
    if i >= eof then
      eof, None
    else
      match read i with
      | '\n' (* LF *) | '\x0b' (* VT *) | '\x0c' (* FF *) ->
        i, Some 1
      | '\r' ->
        if i+1 < eof && read (i+1) = '\n'
        then i, Some 2 (* CRLF *)
        else i, Some 1 (* CR *)
      | '\xc2' ->
        if i+1 < eof && read (i+1) = '\x85'
        then i, Some 2 (* NEL *)
        else (go[@tailcall]) (i+1)
      | '\xe2' ->
        if i+2 < eof && read (i+1) = '\x80' &&
           (match read (i+2) with '\xa8' | '\xa9' -> true | _ -> false)
        then i, Some 3 (* LS and PS *)
        else (go[@tailcall]) (i+1)
      | _ ->
        (go[@tailcall]) (i+1)
  in
  go i

let find_eol ~line_breaks =
  match line_breaks with
  | `Unicode -> find_eol_unicode
  | `Traditional -> find_eol_traditional

let reconstruct_pos ~source ~line_breaks ~eof read offset =
  let find_eol = find_eol ~line_breaks read in
  if offset < 0 then raise @@ Invalid_offset (`Negative offset);
  if offset > eof then raise @@ Invalid_offset (`Beyond_end_of_file (offset, eof));
  let rec go line_num start_of_line =
    let eol, eol_shift = find_eol (start_of_line, eof) in
    if offset <= eol then
      (line_num, start_of_line)
    else
      match eol_shift with
      | None -> assert false;
      | Some s ->
        if offset < eol + s then raise @@ Invalid_offset (`Within_newline (offset, (eol, eol+s)));
        (go[@tailcall]) (line_num+1) (eol+s)
  in
  let line_num, start_of_line = go 1 0 in
  {Range.source = source; offset; start_of_line; line_num}

let check_pos ~line_breaks ~eof read pos =
  let new_pos =
    try reconstruct_pos ~source:pos.Range.source ~line_breaks ~eof read pos.offset
    with Invalid_offset reason -> raise @@ Invalid_position (`Offset reason)
  in
  if pos.line_num <> new_pos.line_num then
    raise @@ Invalid_position (`Incorrect_line_num (pos.line_num, new_pos.line_num));
  if pos.start_of_line <> new_pos.start_of_line then
    raise @@ Invalid_position (`Incorrect_start_of_line (pos.start_of_line, new_pos.start_of_line))

let check_range ~line_breaks ~eof read range =
  let p1, p2 = Range.split range in
  (try check_pos ~line_breaks ~eof read p1 with Invalid_position reason -> raise @@ Invalid_range (`Begin reason));
  (try check_pos ~line_breaks ~eof read p2 with Invalid_position reason -> raise @@ Invalid_range (`End reason))

let replace_control ~tab_size str =
  let tab_string = String.make tab_size ' ' in
  let len = String.length str in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let decoder = String.get_utf_8_uchar str !i in
    if not (Uchar.utf_decode_is_valid decoder) then
      Buffer.add_utf_8_uchar buf Uchar.rep
    else begin
      let u = Uchar.utf_decode_uchar decoder in
      let u_code = Uchar.to_int u in
      if u_code = 0x09 then
        Buffer.add_string buf tab_string
      else if u_code <= 0x1f || 0x7f <= u_code && u_code <= 0x9f || u_code = 0x2028 || u_code = 0x2029 then
        Buffer.add_utf_8_uchar buf Uchar.rep
      else
        Buffer.add_utf_8_uchar buf u
    end;
    i := !i + Uchar.utf_decode_length decoder
  done;
  Buffer.contents buf
