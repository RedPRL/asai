module type S =
sig
  (** [length file_path] gets the size of the file. *)
  val length : string -> int

  (** [unsafe_get file_path i] reads the ith byte of the file without checking the file size. *)
  val unsafe_get : string -> int -> char

  (** [run f] runs the thunk [f] and handles the internal algebraic effects. *)
  val run : (unit -> 'a) -> 'a
end

module File : S
