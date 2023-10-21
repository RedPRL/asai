(** An abstract type of sources. *)
type source

(** [load source] loads the [source]. *)
val load : Range.source -> source

(** [length source] gets the size of the source content. *)
val length : source -> int

(** [unsafe_get source i] reads the ith byte of the file without checking the file size. *)
val unsafe_get : source -> int -> char

(** [run f] runs the thunk [f] and handles the internal algebraic effects. *)
val run : (unit -> 'a) -> 'a
