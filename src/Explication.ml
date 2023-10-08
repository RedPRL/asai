include ExplicationData

let style s v = {style = s; value = v}

let dump_seg dump_style fmt {style; value} =
  Format.fprintf fmt {|@[<1>{style=%a;@,@[<2>value=@,"%s"@]}@]|} dump_style style (String.escaped value)

let pp_list p fmt l =
  Format.fprintf fmt "@[<hv1>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") p)
    l

let dump_line dump_style = pp_list (dump_seg dump_style)

let dump_block dump_style fmt {start_line_num; lines} =
  Format.fprintf fmt {|@[<1>{start_line_num=%d;@,@[<2>lines=@,%a@]}@]|}
    start_line_num
    (pp_list (dump_line dump_style)) lines

let dump_part dump_style fmt {source; blocks} =
  Format.fprintf fmt {|@[<1>{@[<2>source=@,%a@];@,@[<2>blocks=@,%a@]}@]|}
    Span.dump_source source
    (pp_list (dump_block dump_style)) blocks

let dump dump_style = pp_list (dump_part dump_style)
