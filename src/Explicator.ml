open Bwd
open Bwd.Infix

include ExplicatorData
include ExplicatorSigs

let to_start_of_line (pos : Span.position) = {pos with offset = pos.start_of_line}

module Make (R : Reader) (Style : Style) = struct
  type position = Span.position

  (** [find_eol pos] finds the position of the next ['\n']. If the end of file is reached before ['\n'], then the position of the end of the file is returned. *)
  let find_eol ~eof (pos : position) : position =
    assert (pos.offset <= eof);
    let[@tailcall] rec go i =
      if i = eof || R.unsafe_get pos.file_path i = '\n' then
        {pos with offset = i}
      else
        go (i+1)
    in
    go pos.offset

  (** Skip the ['\n'] character, assuming that [eol] is not the end of file *)
  let eol_to_next_line (pos : position) : position =
    { file_path = pos.file_path;
      (* Need to update our offset to skip the newline char *)
      offset = pos.offset + 1;
      start_of_line = pos.offset + 1;
      line_num = pos.line_num + 1 }

  let read_between (begin_ : position) (end_ : position) : string =
    String.init (end_.offset - begin_.offset) @@ fun i ->
    R.unsafe_get begin_.file_path (begin_.offset + i)

  exception UnexpectedLineNumIncrement of Span.position
  exception PositionBeyondEndOfFile of Span.position

  let explicate_block : (Span.position, Style.t) styled list -> Style.t block =
    function
    | [] -> invalid_arg "explicate_block"
    | (p :: _) as ps ->
      let eof = R.length p.value.file_path in
      let start_pos = to_start_of_line p.value in
      let[@tailcall] rec go ~lines ~segments (cur : (Span.position, Style.t) styled) : (Span.position, Style.t) styled list -> _ =
        function
        | p::ps when cur.value.line_num = p.value.line_num ->
          if p.value.offset > eof then raise @@ PositionBeyondEndOfFile p.value;
          (* Still on the same line *)
          let segments = segments <: {style = cur.style; value = read_between cur.value p.value} in
          go ~lines ~segments p ps
        | ps ->
          (* Shifting to the next line *)
          let eol = find_eol ~eof cur.value in
          let segments = segments <: {style = cur.style; value = read_between cur.value eol} in
          let lines = lines <: Bwd.to_list segments in
          (* Continue the process if [is_eof] is false and [ps] is not empty. *)
          match (eol.offset = eof), ps with
          | _, [] ->
            assert (Style.is_default cur.style); lines
          | true, [p] when p.value.offset = eof ->
            assert (Style.is_default p.style); lines
          | true, (p :: _) ->
            raise @@ PositionBeyondEndOfFile p.value
          | false, ps ->
            go ~lines ~segments:Emp {style = cur.style; value = eol_to_next_line eol} ps
      in
      { start_line_num = start_pos.line_num
      ; lines = Bwd.to_list @@ go ~lines:Emp ~segments:Emp {style = Style.default; value = start_pos} ps
      }

  let explicate_blocks = List.map explicate_block

  let explicate_part (file_path, bs) : Style.t part =
    { file_path; blocks = explicate_blocks bs }

  module F = Flattener.Make(Style)

  let explicate ?(splitting_threshold=0) spans =
    List.map explicate_part @@ F.flatten ~splitting_threshold spans
end
