include module type of SourceMarkerSigs

(** Making a marker. *)
module Make : functor (Tag : Tag) -> S with module Tag := Tag
