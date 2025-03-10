type t = [ `Main_message | `Extra_remark of int ] * Text.t
let priority =
  function
  | `Main_message, _ -> -1
  | `Extra_remark i, _ -> i
let compare t1 t2 =
  Utils.compare_pair Int.compare Stdlib.compare
    (priority t1, t1) (priority t2, t2)
let dump fmt =
  function
  | `Main_message, _ -> Format.pp_print_string fmt "`Main_message"
  | `Extra_remark i, _ -> Format.fprintf fmt "`Extra_remark %d" i
