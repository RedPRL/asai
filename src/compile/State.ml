module FileMap = Map.Make(String)
type t = FileState.t FileMap.t

let empty = FileMap.empty

let add op sp m =
  m |> FileMap.update (Asai.Span.file_path sp) @@ fun fs ->
  let fs = Option.value ~default:FileState.empty fs in
  Some (FileState.add (op, sp) fs)
