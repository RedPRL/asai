module Tag_map = Map.Make(Tty_tag)
type t = int Tag_map.t
let empty : t = Tag_map.empty
let is_empty : t -> bool = Tag_map.is_empty
let add t =
  Tag_map.update t @@ function
  | None -> Some 1
  | Some n -> Some (n+1)
let remove t =
  Tag_map.update t @@ function
  | None -> failwith "Asai.Tty.S.display: removing a non-existing tag from a tag set"
  | Some 1 -> None
  | Some n -> Some (n-1)
let prioritized s = Option.map fst @@ Tag_map.min_binding_opt s
