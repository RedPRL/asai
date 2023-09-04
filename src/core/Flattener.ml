open Bwd
open Bwd.Infix

open Explicated

type block = Span.position styled list
type section = string * block list

type 'a marked =
  { value : 'a
  ; marks : section list
  }

module File =
struct
  module Style = Accumulator.Make(Highlighting)
  module PositionMap = Map.Make(Span.SingleFilePosition)

  type t = Style.t PositionMap.t

  let empty : t = PositionMap.empty

  let add_pos pos value : t -> t =
    PositionMap.update pos @@ function
    | None -> Some (Style.singleton value)
    | Some s -> Some (Style.add value s)

  (* invariant: all spans added to the same [t] should have the same filename *)
  let add st sp m : t =
    let begin_, end_ = Span.to_positions sp in
    add_pos begin_ (st, 1) @@ add_pos end_ (st, -1) m

  let singleton st sp : t = add st sp empty

  let render t : Span.position styled list =
    let acc, attrs =
      PositionMap.fold
        (fun position attrs (acc, prev_attrs) ->
           let attrs = Style.merge prev_attrs attrs in
           let style = Style.render attrs in
           let acc = acc <: {style; value = position} in
           (acc, attrs))
        t (Emp, Style.empty)
    in
    assert (Style.render attrs = None);
    Utils.keep_first (fun x y -> Option.equal Highlighting.equal x.style y.style) @@ Bwd.to_list acc
end

module Files = struct
  module FileMap = Map.Make(String)
  type t = File.t FileMap.t

  let empty : t = FileMap.empty

  let add st sp =
    FileMap.update (Span.file_path sp) @@ function
    | None -> Some (File.singleton st sp)
    | Some m -> Some (File.add st sp m)

  let singleton st sp = add st sp empty

  let is_primary_point =
    function
    | {style = Some s; _} when Highlighting.is_primary s -> true
    | _ -> false

  let is_primary_file = List.exists is_primary_point

  let render m : (string * block) list =
    let m = FileMap.map File.render m in
    let primary_files, other_files = FileMap.partition (fun _ -> is_primary_file) m in
    FileMap.bindings primary_files @ FileMap.bindings other_files
end

let split_block ~splitting_threshold =
  Utils.group @@ fun p q ->
  p.style <> None ||
  p.value.Span.line_num - q.value.line_num <= splitting_threshold

let split_section ~splitting_threshold (file, block) =
  file, split_block ~splitting_threshold block

let all_marks ~additional_marks loc =
  Option.to_list (Option.map (fun loc -> `Primary, loc) loc) @
  List.map (fun sp -> `Related, sp) additional_marks

let flatten ~splitting_threshold ~additional_marks loc =
  List.map (fun (f, b) -> f, split_block ~splitting_threshold b) @@
  Files.render @@
  List.fold_left (fun f (st, sp) -> Files.add st sp f) Files.empty @@
  all_marks ~additional_marks loc
