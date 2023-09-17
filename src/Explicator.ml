open Bwd
open Bwd.Infix

include ExplicatorData
include ExplicatorSigs

module Make (R : Reader) (Style : Style) = struct
  type position = Span.position

  (** [find_eol pos] finds the position of the next ['\n']. If the end of file is reached before ['\n'], then the position of the end of the file is returned. *)
  let find_eol (pos : position) : position =
    let eof = R.length pos.file_path in
    assert (pos.offset <= eof);
    let[@tailcall] rec go i =
      if i = eof || R.unsafe_get pos.file_path i = '\n' then
        {pos with offset = i}
      else
        go (i+1)
    in
    go pos.offset

  (** Skip the ['\n'] character. *)
  let eol_to_next_line (pos : position) : position =
    { pos with
      (* Need to update our offset to skip the newline char *)
      offset = pos.offset + 1;
      start_of_line = pos.offset + 1;
      line_num = pos.line_num + 1 }

  let read_between (begin_ : position) (end_ : position) : string =
    String.init (end_.offset - begin_.offset) @@ fun i ->
    R.unsafe_get begin_.file_path (begin_.offset + i)

  let explicate_block : (Span.position, Style.t) styled list -> Style.t block =
    function
    | [] -> invalid_arg "explicate_block"
    | (b :: _) as bs ->
      let start_pos = Span.to_start_of_line b.value in
      let[@tailcall] rec go ~lines ~segments (cur : _ styled) : _ styled list -> _ =
        function
        | p::ps when cur.value.Span.line_num = p.value.Span.line_num ->
          (* Still on the same line *)
          let segments = segments <: {style = cur.style; value = read_between cur.value p.value} in
          go ~lines ~segments p ps
        | ps ->
          (* Shifting to the next line *)
          let eol = find_eol cur.value in
          let segments = segments <: {style = cur.style; value = read_between cur.value eol} in
          let lines = lines <: Bwd.to_list segments in
          (* Continue the process if [ps] is not empty. *)
          match ps with
          | [] -> assert (Style.is_none cur.style); lines
          | _ -> go ~lines ~segments:Emp {style = cur.style; value = eol_to_next_line eol} ps
      in
      { start_line_num = start_pos.line_num
      ; lines = Bwd.to_list @@ go ~lines:Emp ~segments:Emp {style = Style.none; value = start_pos} bs
      }

  let explicate_blocks = List.map explicate_block

  let explicate_part (file_path, bs) : Style.t part =
    { file_path; blocks = explicate_blocks bs }

  module F = Flattener.Make(Style)

  let explicate ?(splitting_threshold=0) spans =
    List.map explicate_part @@ F.flatten ~splitting_threshold spans
end
