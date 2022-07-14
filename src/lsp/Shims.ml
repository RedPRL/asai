open Asai
open Lsp.Types

module Loc =
struct
  let lsp_pos_of_pos (pos : Pos.t) =
    Position.create
      ~line:(Pos.line pos - 1)
      ~character:(Pos.offset pos)

  let lsp_range_of_span (sp : Span.t) =
    Range.create
      ~start:(lsp_pos_of_pos @@ Span.start_pos sp)
      ~end_:(lsp_pos_of_pos @@ Span.stop_pos sp)
end

module Diagnostic =
struct
  let lsp_severity_of_severity =
    function
    | Severity.Info -> DiagnosticSeverity.Information
    | Severity.Warning -> DiagnosticSeverity.Warning
    | Severity.Error -> DiagnosticSeverity.Error
    | Severity.Panic -> DiagnosticSeverity.Error
end
