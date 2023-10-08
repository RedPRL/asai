open ExplicationData
open ExplicatorSigs

type 'style block = (Span.position, 'style) styled list
type 'style part = Span.source * 'style block list

module File (Style : Style) :
sig
  type t
  val empty : t [@@warning "-32"]
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
  let (=) x y = x.Span.offset = y.Span.offset [@@warning "-32"]

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
    go Style.default

  let add {value; style} l =
    let x1, x2 = Span.split value in
    impose {value = x1, x2; style} @@ add_point x1 @@ add_point x2 l

  let singleton data = add data empty

  let render l =
    l
    |> Utils.drop_while (fun x -> Style.is_default x.style)
    |> Utils.keep_first_in_groups (fun x y -> Style.equal x.style y.style)
end

module Files (Style : ExplicatorSigs.Style) :
sig
  type t
  val empty : t
  val add : (Span.t, Style.t) styled -> t -> t
  val render : t -> (Span.source * Style.t block) list
end
=
struct
  module FileMap = Map.Make(struct
      type t = Span.source
      let compare : t -> t -> int = compare
    end)
  module F = File(Style)
  type t = F.t FileMap.t

  let empty : t = FileMap.empty

  let add data =
    FileMap.update (Span.source data.value) @@ function
    | None -> Some (F.singleton data)
    | Some m -> Some (F.add data m)

  let max_style l : Style.t = List.fold_left (fun s x -> Style.max s x.style) Style.default l

  let render m : (Span.source * Style.t block) list =
    FileMap.bindings m
    |> List.map (fun (src, x) -> src, F.render x)
    |> List.filter (fun (_, l) -> l <> []) (* filter out sources with only empty spans *)
    |> List.map (fun (src, l) -> src, (max_style l, l)) (* calculate the importance *)
    |> List.stable_sort (fun (_, (sx, _)) (_, (sy, _)) -> Style.compare sx sy)
    |> List.map (fun (src, (_, block)) -> src, block)
end

module Splitter (Style : Style) :
sig
  val split : block_splitting_threshold:int -> Style.t block -> Style.t block list
end
=
struct
  let split ~block_splitting_threshold =
    Utils.group @@ fun p q ->
    not (Style.is_default p.style) ||
    q.value.Span.line_num - p.value.line_num <= block_splitting_threshold
end

module Make (Style : Style) = struct
  module F = Files(Style)
  module S = Splitter(Style)

  (* Currently, this can take \tilde{O}(n^2) time where n is the number of styled spans. *)
  let flatten ~block_splitting_threshold spans : Style.t part list =
    spans
    |> List.fold_left (fun f data -> F.add data f) F.empty
    |> F.render
    |> List.map (fun (src, block) -> src, S.split ~block_splitting_threshold block)
end
