include Asai.Explicator.Reader

(** [run f] runs the thunk [f] and handles the internal algebraic effects. *)
val run : (unit -> 'a) -> 'a
