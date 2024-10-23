include ExplicationData

let dump_marker dump_tag fmt =
  function
  | RangeBegin tag -> Format.fprintf fmt {|@[<2>RangeBegin@ @[%a@]@]|} dump_tag tag
  | RangeEnd tag -> Format.fprintf fmt {|@[<2>RangeEnd@ @[%a@]@]|} dump_tag tag
  | Point tag -> Format.fprintf fmt {|@[<2>Point@ @[%a@]@]|} dump_tag tag

let dump_token dump_tag fmt =
  function
  | String str -> Format.fprintf fmt {|@[<2>String@ "%s"@]|} (String.escaped str)
  | Marker m -> Format.fprintf fmt {|@[<2>Marker@ @[<1>(%a)@]@]|} (dump_marker dump_tag) m

let dump_line dump_tag fmt {markers; tokens} =
  Format.fprintf fmt {|@[<1>{@[<2>markers=@,@[%a@]@];@ @[<2>tokens=@ @[%a@]@]}@]|}
    (Utils.dump_list dump_tag) markers
    (Utils.dump_list (dump_token dump_tag)) tokens

let dump_block dump_tag fmt {begin_line_num; end_line_num; lines} =
  Format.fprintf fmt {|@[<1>{begin_line_num=%d;@ end_line_num=%d;@ @[<2>lines=@ @[%a@]@]}@]|}
    begin_line_num end_line_num
    (Utils.dump_list (dump_line dump_tag)) lines

let dump_part dump_tag fmt {source; blocks} =
  Format.fprintf fmt {|@[<1>{@[<2>source=@ %a@];@,@[<2>blocks=@ %a@]}@]|}
    Range.dump_source source
    (Utils.dump_list (dump_block dump_tag)) blocks

let dump dump_tag = Utils.dump_list (dump_part dump_tag)
