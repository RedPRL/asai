let keep_first p =
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
