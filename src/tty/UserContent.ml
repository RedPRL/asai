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
