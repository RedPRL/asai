open Bwd
open Bwd.Infix

let dump_string fmt s = Format.fprintf fmt "%S" s [@@inline]

let dump_option dump fmt =
  function
  | None -> Format.pp_print_string fmt "None"
  | Some v -> Format.fprintf fmt "@[<2>Some@ %a@]" dump v

let dump_pair dump_x dump_y fmt (x, y) =
  Format.fprintf fmt {|@[<1>(%a,@ %a)@]|} dump_x x dump_y y

let dump_triple dump_x dump_y dump_z fmt (x, y, z) =
  Format.fprintf fmt {|@[<1>(%a,@ %a,@ %a)@]|} dump_x x dump_y y dump_z z

let dump_list p fmt l =
  Format.fprintf fmt "@[<hv1>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") p)
    l

(* Currently note used

   {[
     let rec drop_while p =
       function
       | x :: xs when p x -> (drop_while[@tailcall]) p xs
       | l -> l
   ]}
*)

let keep_first_in_groups p =
  function
  | [] -> []
  | x :: xs ->
    let[@tail_mod_cons] rec go x =
      function
      | [] -> [x]
      | y :: ys when p x y -> (go[@tailcall]) x ys
      | y :: ys -> x :: (go[@tailcall]) y ys
    in
    go x xs

(* Currently note used

   {[
     let group p =
       function
       | [] -> []
       | x :: xs ->
         let[@tail_mod_cons] rec go acc x =
           function
           | [] -> [acc @> [x]]
           | y :: ys when p x y -> (go[@tailcall]) (acc <: x) y ys
           | y :: ys -> (acc @> [x]) :: (go[@tailcall]) Emp y ys
         in
         go Emp x xs
   ]}
*)

let maximum = List.fold_left Int.max Int.min_int

let compare_pair c1 c2 (x1, y1) (x2, y2) : int =
  match c1 x1 x2 with
  | 0 -> c2 y1 y2
  | r -> r

let compare_opposite c x y = - c x y

let span p =
  let rec go acc =
    function
    | x :: xs when p x -> (go[@tailcall]) (acc <: x) xs
    | l -> Bwd.to_list acc, l
  in
  go Emp
