type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module M = Map.Make (String)
module E = Algaeff.State.Make (struct type t = (Unix.file_descr * bigstring) M.t end)

module File_internal =
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

type source = File of bigstring | String of string

let load : Range.source -> _ =
  function
  | `File file_path -> File (snd @@ File_internal.load file_path)
  | `String {content; _} -> String content

let length =
  function
  | File arr -> Bigarray.Array1.size_in_bytes arr
  | String str -> String.length str

let[@inline] unsafe_get res i =
  match res with
  | File arr -> Bigarray.Array1.unsafe_get arr i
  | String str -> String.unsafe_get str i

let run f =
  E.run ~init:M.empty @@ fun () ->
  Fun.protect ~finally:File_internal.close_all f
