open Asai
module Lsp := Lsp.Types

module Loc : sig
  val lsp_pos_of_pos : Pos.t -> Lsp.Position.t
  val lsp_range_of_span : Span.t -> Lsp.Range.t
end

module Diagnostic : sig
  val lsp_severity_of_severity : Severity.t -> Lsp.DiagnosticSeverity.t
end
