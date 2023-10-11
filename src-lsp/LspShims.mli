module L := Lsp.Types

module Loc : sig
  val lsp_pos_of_pos : Asai.Span.position -> L.Position.t
  val lsp_range_of_span : Asai.Span.t option -> L.Range.t
end

module Diagnostic : sig
  val lsp_severity_of_severity : Asai.Diagnostic.severity -> L.DiagnosticSeverity.t
end
