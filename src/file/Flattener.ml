module StringMap = Map.Make(String)
type t = SectionFlattener.t StringMap.t

let empty = StringMap.empty

let add (op, sp) m =
  m |> StringMap.update (Asai.Span.file_path sp) @@ fun fs ->
  let fs = Option.value ~default:SectionFlattener.empty fs in
  Some (SectionFlattener.add (op, sp) fs)

let singleton x = add x empty

let is_highlighted : _ -> bool =
  function Flattened.{style = Some `Highlighted; _} -> true | _ -> false

let is_highlighted_block = List.exists is_highlighted

let is_highlighted_blocks = List.exists is_highlighted_block

let flatten ~splitting_threshold m : Flattened.section list =
  let m = StringMap.map (SectionFlattener.flatten ~splitting_threshold) m in
  let highlighted_sections, other_sections = StringMap.partition (fun _ -> is_highlighted_blocks) m in
  List.of_seq @@ Seq.append (StringMap.to_seq highlighted_sections) (StringMap.to_seq other_sections)
