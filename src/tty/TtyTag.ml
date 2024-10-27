type index = Main | Extra of int
type t = index * Text.t
let priority =
  function
  | Main, _ -> -1
  | Extra i, _ -> i
let compare t1 t2 =
  Utils.compare_pair Int.compare Stdlib.compare
    (priority t1, t1) (priority t2, t2)
let dump fmt =
  function
  | Main, _ -> Format.pp_print_string fmt "Main"
  | Extra i, _ -> Format.fprintf fmt "Extra %d" i
