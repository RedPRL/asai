type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module M = Map.Make (String)
module E = Algaeff.State.Make (struct type state = (Unix.file_descr * bigstring) M.t end)
module L = Algaeff.Mutex.Make ()

let load file_path =
  L.exclusively @@ fun () ->
  match M.find_opt file_path (E.get ()) with
  | Some (fd, str) -> fd, str
  | None ->
    let fd, str =
      try
        let fd = Unix.openfile file_path [Unix.O_RDONLY] 0o777 in
        let str = Bigarray.array1_of_genarray @@ Unix.map_file fd Bigarray.char Bigarray.c_layout false [|-1|] in
        fd, str
      with _ -> raise @@ Sys_error ("could not open file " ^ file_path)
    in
    E.modify (M.add file_path (fd, str));
    fd, str

let close_all () =
  L.exclusively @@ fun () ->
  M.iter (fun _ (fd, _) -> try Unix.close fd with _ -> ()) (E.get ());
  E.set M.empty

let length file_path =
  let fd, _ = load file_path in
  (Unix.fstat fd).st_size

let unsafe_get file_path i =
  let _, str = load file_path in
  Bigarray.Array1.unsafe_get str i

let run f =
  L.run @@ fun () ->
  E.run ~init:M.empty @@ fun () ->
  Fun.protect ~finally:close_all f
