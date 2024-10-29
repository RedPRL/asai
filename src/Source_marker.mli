include module type of Source_marker_sigs

(** Making a marker. *)
module Make : functor (Tag : Tag) -> S with module Tag := Tag
