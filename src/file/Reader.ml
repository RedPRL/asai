module type S =
sig
  (** [length file_path] gets the size of the file. *)
  val length : string -> int

  (** [unsafe_get file_path i] reads the ith byte of the file without checking the file size. *)
  val unsafe_get : string -> int -> char

  (** [run f] runs the thunk [f] and handles the internal algebraic effects. *)
  val run : (unit -> 'a) -> 'a
end

module File : S =
struct
  type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  module M = Map.Make (String)
  module E = Algaeff.State.Make (struct type state = (Unix.file_descr * bigstring) M.t end)

  module Internal =
  struct
    let load file_path =
      match M.find_opt file_path (E.get ()) with
      | Some (fd, str) -> fd, str
      | None ->
        let fd =
          try Unix.openfile file_path [Unix.O_RDONLY] 0o777
          with _ -> raise @@ Sys_error ("could not open file " ^ file_path)
        in
        let str =
          try Bigarray.array1_of_genarray @@ Unix.map_file fd Bigarray.char Bigarray.c_layout false [|-1|]
          with _ ->
            (* the fd is already open! *)
            (try Unix.close fd with _ -> ());
            raise @@ Sys_error ("could not read file " ^ file_path)
        in
        E.modify (M.add file_path (fd, str));
        fd, str

    let close_all () =
      M.iter (fun _ (fd, _) -> try Unix.close fd with _ -> ()) (E.get ());
      E.set M.empty
  end

  let length file_path =
    let fd, _ = Internal.load file_path in
    (Unix.fstat fd).st_size

  let unsafe_get file_path i =
    let _, str = Internal.load file_path in
    Bigarray.Array1.unsafe_get str i

  let run f =
    E.run ~init:M.empty @@ fun () ->
    Fun.protect ~finally:Internal.close_all f
end
