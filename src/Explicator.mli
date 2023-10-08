include module type of ExplicatorSigs

(** Making an explicator. *)
module Make : functor (Style : Style) -> S with module Style := Style
