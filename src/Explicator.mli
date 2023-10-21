include module type of ExplicatorSigs

(* The default tag blending algorithm that chooses the more important tag based on priority. *)
val default_blend : priority:('tag -> int) -> 'tag -> 'tag -> 'tag

(** Making an explicator. *)
module Make : functor (Tag : Tag) -> S with module Tag := Tag
