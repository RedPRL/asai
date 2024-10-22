include module type of ExplicatorSigs

(** Making an explicator. *)
module Make : functor (Tag : Tag) -> S with module Tag := Tag
