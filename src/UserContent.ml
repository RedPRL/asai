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

let find_eol ~line_breaking =
  match line_breaking with
  | `Unicode -> find_eol_unicode
  | `Traditional -> find_eol_traditional

let count_newlines ~line_breaking read (pos, eof) =
  let find_eol i = find_eol ~line_breaking read (i, eof) in
  let rec go acc i =
    match find_eol i with
    | _, None -> acc
    | pos, Some shift -> (go[@tailcall]) (acc+1) (pos+shift)
  in
  go 0 pos

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
