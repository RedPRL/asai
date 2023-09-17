open ExplicatorData
open ExplicatorSigs

type 'style block = (Span.position, 'style) styled list
type 'style part = string * 'style block list

module File (Style : Style) :
sig
  type t
  val empty : t
  val singleton : (Span.t, Style.t) styled -> t
  val add : (Span.t, Style.t) styled -> t -> t
  val render : t -> Style.t block
end
=
struct
  type t = (Span.position, Style.t) styled list

  let empty : t = []

  let (<) x y = x.Span.offset < y.Span.offset
  let (<=) x y = x.Span.offset <= y.Span.offset
  let (=) x y = x.Span.offset = y.Span.offset

  (* precondition: x1 < x2 and there are already points at x1 and x2 *)
  let impose {value = x1, x2; style = xst} : _ list -> _ list =
    List.map
      (fun y ->
         if x1 <= y.value && y.value < x2
         then {y with style = Style.compose y.style xst}
         else y)

  let add_point x =
    let[@tail_mod_cons] rec go last =
      function
      | [] -> [{value = x; style = last}]
      | y :: ys when y.value < x ->
        y :: (go[@tailcall]) y.style ys
      | y :: ys when x < y.value ->
        {value = x; style = last} :: y :: ys
      | l -> l
    in
    go Style.none

  let add {value; style} l =
    let x1, x2 = Span.to_positions value in
    impose {value = x1, x2; style} @@ add_point x1 @@ add_point x2 l

  let singleton data = add data empty

  let render l =
    l
    |> Utils.drop_while (fun x -> Style.is_none x.style)
    |> Utils.keep_first_in_groups (fun x y -> Style.equal x.style y.style)
end

module Files (Style : ExplicatorSigs.Style) :
sig
  type t
  val empty : t
  val add : (Span.t, Style.t) styled -> t -> t
  val render : t -> (string * Style.t block) list
end
=
struct
  module FileMap = Map.Make(String)
  module F = File(Style)
  type t = F.t FileMap.t

  let empty : t = FileMap.empty

  let add data =
    FileMap.update (Span.file_path data.value) @@ function
    | None -> Some (F.singleton data)
    | Some m -> Some (F.add data m)

  let max_style l : Style.t = List.fold_left (fun s x -> Style.max s x.style) Style.none l

  let render m : (string * Style.t block) list =
    FileMap.bindings m
    |> List.map (fun (f, x) -> f, F.render x)
    |> List.filter (fun (_, l) -> l <> []) (* filter out files with only empty spans *)
    |> List.map (fun (f, l) -> f, (max_style l, l)) (* calculate the importance *)
    |> List.stable_sort (fun (_, (sx, _)) (_, (sy, _)) -> Style.compare sx sy)
    |> List.map (fun (file_path, (_, block)) -> file_path, block)
end

module Splitter (Style : Style) :
sig
  val split : splitting_threshold:int -> Style.t block -> Style.t block list
end
=
struct
  let split ~splitting_threshold =
    Utils.group @@ fun p q ->
    not (Style.is_none p.style) ||
    p.value.Span.line_num - q.value.line_num <= splitting_threshold
end

module Make (Style : Style) = struct
  module F = Files(Style)
  module S = Splitter(Style)

  let flatten ~splitting_threshold spans : Style.t part list =
    spans
    |> List.fold_left (fun f data -> F.add data f) F.empty
    |> F.render
    |> List.map (fun (file_path, block) -> file_path, S.split ~splitting_threshold block)
end
