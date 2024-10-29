type t = int * string
let equal = (=)
let priority (i, _) = -i
let dump fmt (i, s) = Format.fprintf fmt {|(%d, "%s")|} i (String.escaped s)
