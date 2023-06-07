module type S =
sig
  val mark_section : Flattened.section -> MarkedDiagnostic.section
end

module Make (R : Reader.S) : S
