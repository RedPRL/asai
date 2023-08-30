module type S =
sig
  type t
  type trait
  val empty : t
  val singleton : trait * int -> t
  val add : trait * int -> t -> t
  val merge : t -> t -> t
  val render : t -> trait option
end

module Make (Trait : Map.OrderedType) : S with type trait := Trait.t =
struct
  module TraitMap = Map.Make(Trait)
  type t = int TraitMap.t (* invariant: all numbers are non-zeros *)
  let empty = TraitMap.empty
  let singleton (t, i) =
    if i = 0 then empty else TraitMap.singleton t i
  let merge : t -> t -> t =
    TraitMap.union @@ fun _ i1 i2 ->
    let i = i1 + i2 in
    if i = 0 then None else Some i
  let add p t = merge t (singleton p)
  let render m = Option.map fst @@ TraitMap.max_binding_opt m
end
