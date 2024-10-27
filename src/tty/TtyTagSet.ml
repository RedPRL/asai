module TagMap = Map.Make(TtyTag)
type t = int TagMap.t
let empty : t = TagMap.empty
let is_empty : t -> bool = TagMap.is_empty
let add t =
  TagMap.update t @@ function
  | None -> Some 1
  | Some n -> Some (n+1)
let remove t =
  TagMap.update t @@ function
  | None -> failwith "Asai.Tty.S.display: removing a non-existing tag from a tag set"
  | Some 1 -> None
  | Some n -> Some (n-1)
let prioritized s = Option.map fst @@ TagMap.min_binding_opt s
