module FileMap = Map.Make(String)
type t = PerFileFlatter.t FileMap.t

let empty = FileMap.empty

let add (op, sp) m =
  m |> FileMap.update (Asai.Span.file_path sp) @@ fun fs ->
  let fs = Option.value ~default:PerFileFlatter.empty fs in
  Some (PerFileFlatter.add (op, sp) fs)

let singleton x = add x empty

let is_highlighted : Marked.style option * OrderedPosition.t -> bool =
  function Some `Highlighted, _ -> true | _ -> false

let is_highlighted_block = List.exists is_highlighted

let is_highlighted_blocks = List.exists is_highlighted_block

let flatten ~splitting_threshold m : Flattened.file list =
  let m = FileMap.map (PerFileFlatter.flatten ~splitting_threshold) m in
  let highlighted_files, other_files = FileMap.partition (fun _ -> is_highlighted_blocks) m in
  List.of_seq @@ Seq.append (FileMap.to_seq highlighted_files) (FileMap.to_seq other_files)
