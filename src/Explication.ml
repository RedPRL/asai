include ExplicationData

let dump_seg dump_tag = Utils.dump_pair (Utils.dump_option dump_tag) Utils.dump_string

let dump_line dump_tag fmt {tags; segments} =
  Format.fprintf fmt {|@[<1>{@[<2>tags=@,@[%a@]@];@ @[<2>segments=@ @[%a@]@]}@]|}
    (Utils.dump_list dump_tag) tags
    (Utils.dump_list (dump_seg dump_tag)) segments

let dump_block dump_tag fmt {begin_line_num; end_line_num; lines} =
  Format.fprintf fmt {|@[<1>{begin_line_num=%d;@ end_line_num=%d;@ @[<2>lines=@ @[%a@]@]}@]|}
    begin_line_num end_line_num
    (Utils.dump_list (dump_line dump_tag)) lines

let dump_part dump_tag fmt {source; blocks} =
  Format.fprintf fmt {|@[<1>{@[<2>source=@ %a@];@,@[<2>blocks=@ %a@]}@]|}
    Range.dump_source source
    (Utils.dump_list (dump_block dump_tag)) blocks

let dump dump_tag = Utils.dump_list (dump_part dump_tag)
