open Bwd
open Bwd.Infix

open Context

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

  let flatten t : Span.position styled list =
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

let flatten m : (string * Span.position styled list) list =
  let m = FileMap.map File.flatten m in
  let primary_files, other_files = FileMap.partition (fun _ -> is_primary_file) m in
  FileMap.bindings primary_files @ FileMap.bindings other_files
