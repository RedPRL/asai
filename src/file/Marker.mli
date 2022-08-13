module type S =
sig
  val mark_section : Flattened.section -> Marked.section
end

module Make (R : Reader.S) : S
