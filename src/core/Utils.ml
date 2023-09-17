open Bwd
open Bwd.Infix

let rec drop_while p =
  function
  | x :: xs when p x -> (drop_while[@tailcall]) p xs
  | l -> l

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
