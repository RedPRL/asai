open Asai
open Lsp.Types

module Loc =
struct
  let lsp_pos_of_pos (pos : Span.position) =
    Position.create
      ~line:(pos.line_num - 1)
      ~character:(pos.offset - pos.start_of_line)

  let lsp_range_of_span (sp : Span.t option) =
    match sp with
    | Some sp ->
      let (start , stop) = Span.to_positions sp in
      Range.create
        ~start:(lsp_pos_of_pos start)
        ~end_:(lsp_pos_of_pos stop)
    | None ->
      (* When we have a message without a location,
         we set it's location to the start of the file,
         as we don't have any better choices. *)
      let start_of_file = Position.create ~line:0 ~character:0 in
      Range.create ~start:start_of_file ~end_:start_of_file
end

module Diagnostic =
struct
  let lsp_severity_of_severity =
    function
    | Severity.Hint -> DiagnosticSeverity.Hint
    | Severity.Info -> DiagnosticSeverity.Information
    | Severity.Warning -> DiagnosticSeverity.Warning
    | Severity.Error -> DiagnosticSeverity.Error
    | Severity.Bug -> DiagnosticSeverity.Error
end
