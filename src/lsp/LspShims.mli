module L := Lsp.Types

module Loc : sig
  val lsp_pos_of_pos : Span.position -> L.Position.t
  val lsp_range_of_span : Span.t option -> L.Range.t
end

module Diagnostic : sig
  val lsp_severity_of_severity : Diagnostic.severity -> L.DiagnosticSeverity.t
end
