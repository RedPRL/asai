module L = Lsp.Types

module Loc =
struct
  let lsp_pos_of_pos (pos : Asai.Range.position) =
    L.Position.create
      ~line:(pos.line_num - 1)
      ~character:(pos.offset - pos.start_of_line)

  let lsp_range_of_range (r : Asai.Range.t option) =
    match r with
    | Some r ->
      let (start , stop) =
        match Asai.Range.view r with
        | `Range (start, stop) -> start, stop
        | `End_of_file pos -> pos, pos
      in
      L.Range.create
        ~start:(lsp_pos_of_pos start)
        ~end_:(lsp_pos_of_pos stop)
    | None ->
      (* When we have a message without a location,
         we set it's location to the start of the file,
         as we don't have any better choices. *)
      let start_of_file = L.Position.create ~line:0 ~character:0 in
      L.Range.create ~start:start_of_file ~end_:start_of_file
end

module Diagnostic =
struct
  let lsp_severity_of_severity : Asai.Diagnostic.severity -> L.DiagnosticSeverity.t =
    function
    | Hint -> Hint
    | Info -> Information
    | Warning -> Warning
    | Error -> Error
    | Bug -> Error
end
