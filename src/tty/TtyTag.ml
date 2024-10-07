type index = Main | Extra of int
type t = index * Text.t
let equal (x : t) y = fst x = fst y
let priority =
  function
  | Main, _ -> -1
  | Extra i, _ -> i
let dump fmt =
  function
  | Main, _ -> Format.pp_print_string fmt "Main"
  | Extra i, _ -> Format.fprintf fmt "Extra %d" i
