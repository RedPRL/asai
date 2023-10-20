include ExplicationData

let dump_seg dump_tag fmt (tag, value) =
  Format.fprintf fmt {|@[<1>(%a,@ %a)@]|} (Utils.dump_option dump_tag) tag Utils.dump_string value

let dump_line dump_tag fmt {tags=_; segments} =
  Format.fprintf fmt {|@[<1>{tags=...;@,@[<2>segments=@,%a@]}@]|}
    (Utils.pp_list (dump_seg dump_tag)) segments

let dump_block dump_tag fmt {begin_line_num; end_line_num; lines} =
  Format.fprintf fmt {|@[<1>{begin_line_num=%d;@,end_line_num=%d;@,@[<2>lines=@,%a@]}@]|}
    begin_line_num end_line_num
    (Utils.pp_list (dump_line dump_tag)) lines

let dump_part dump_tag fmt {source; blocks} =
  Format.fprintf fmt {|@[<1>{@[<2>source=@,%a@];@,@[<2>blocks=@,%a@]}@]|}
    Span.dump_source source
    (Utils.pp_list (dump_block dump_tag)) blocks

let dump dump_tag = Utils.pp_list (dump_part dump_tag)
