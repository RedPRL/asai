open Asai

type tm = tm_ Range.located
and tm_ =
  | Var of string
  | Lam of string * tm
  | Ap of tm * tm
  | Pair of tm * tm
  | Fst of tm
  | Snd of tm
  | Lit of int
  | Suc of tm
  | Nat_rec of tm * tm * tm

and tp =
  | Fun of tp * tp
  | Tuple of tp * tp
  | Nat

let rec pp_tp fmt =
  function
  | Fun (a, b) ->
    Format.fprintf fmt "(→ %a %a)"
      pp_tp a
      pp_tp b
  | Tuple (a, b) ->
    Format.fprintf fmt "(× %a %a)"
      pp_tp a
      pp_tp b
  | Nat ->
    Format.fprintf fmt "ℕ"
