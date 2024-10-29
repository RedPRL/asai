module L := Lsp.Types

module Loc : sig
  val lsp_pos_of_pos : Asai.Range.position -> L.Position.t
  val lsp_range_of_range : Asai.Range.t option -> L.Range.t
end

module Diagnostic : sig
  val lsp_severity_of_severity : Asai.Diagnostic.severity -> L.DiagnosticSeverity.t
end
