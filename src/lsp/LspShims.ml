module L = Lsp.Types

module Loc =
struct
  let lsp_pos_of_pos (pos : Span.position) =
    L.Position.create
      ~line:(pos.line_num - 1)
      ~character:(pos.offset - pos.start_of_line)

  let lsp_range_of_span (sp : Span.t option) =
    match sp with
    | Some sp ->
      let (start , stop) = Span.split sp in
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
  let lsp_severity_of_severity =
    function
    | Diagnostic.Hint -> L.DiagnosticSeverity.Hint
    | Diagnostic.Info -> L.DiagnosticSeverity.Information
    | Diagnostic.Warning -> L.DiagnosticSeverity.Warning
    | Diagnostic.Error -> L.DiagnosticSeverity.Error
    | Diagnostic.Bug -> L.DiagnosticSeverity.Error
end
