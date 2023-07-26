open Bwd
open Bwd.Infix

open Asai

type segment = string Flattener.styled

type line = segment list

type block =
  { start_line_num : int
  ; lines : line list
  }

type section =
  { file_path : string
  ; blocks : block list
  }

type 'a contextualized =
  { value : 'a
  ; context : section list
  }

type 'code t =
  { code : 'code (** The error code. *)
  ; severity : Severity.t (** The severity of the message. *)
  ; message : Diagnostic.message contextualized (** The marked message. *)
  ; backtrace : Diagnostic.message contextualized bwd
  }

module Make (R : Reader.S) :
sig
  val contextualize : splitting_threshold:int -> 'code Asai.Diagnostic.t -> 'code t
end

= struct
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

  let read_between (begin_, end_ : position * position) : string =
    String.init (end_.offset - begin_.offset) @@ fun i ->
    R.unsafe_get begin_.file_path (begin_.offset + i)

  let is_empty_span (begin_, end_ : position * position) : bool =
    begin_.offset = end_.offset

  let append_segment (segments : segment bwd) Flattener.{style; value = span} : segment bwd =
    if is_empty_span span
    then segments
    else segments <: {style; value = read_between span}

  let contextualize_block : Span.position Flattener.styled list -> block =
    let open Asai in
    function
    | [] -> invalid_arg "contextualize_block"
    | (b :: _) as bs ->
      let start_pos = Span.to_start_of_line b.value in
      let[@tailcall] rec go ~lines ~segments Flattener.{style; value = cursor} : position Flattener.styled list -> line bwd =
        function
        | p::ps when cursor.Span.line_num = p.Flattener.value.Span.line_num ->
          let segments = append_segment segments {style; value = cursor, p.value} in
          go ~lines ~segments p ps
        | ps ->
          (* Shift to the next line *)
          let eol = find_eol cursor in
          let segments = append_segment segments {style; value = cursor, eol} in
          let lines = lines <: Bwd.to_list segments in
          (* Continue the process if [ps] is not empty. *)
          if ps = [] then lines
          else go ~lines ~segments:Emp {style; value = eol_to_next_line eol} ps
      in
      { start_line_num = start_pos.line_num
      ; lines = Bwd.to_list @@ go ~lines:Emp ~segments:Emp {style = None; value = start_pos} bs
      }

  let contextualize_blocks = List.map contextualize_block
  let contextualize_section (file_path, bs) : section =
    { file_path; blocks = contextualize_blocks bs }

  let group p =
    function
    | [] -> []
    | x :: xs ->
      let[@tail_mod_cons] rec go acc x =
        function
        | [] -> [acc @> [x]]
        | y :: ys when p x y -> (go[@tailcall]) (acc <: x) y ys
        | y :: ys -> (acc @> [x]) :: (go[@tailcall]) Emp y ys
      in
      go Emp x xs

  let split_block ~splitting_threshold : Span.position Flattener.styled list -> _ list list =
    group @@ fun p q -> p.Flattener.value.Span.line_num - q.value.line_num <= splitting_threshold || p.style <> None

  let split_section ~splitting_threshold (file, block) = file, split_block ~splitting_threshold block

  let contextualize_located ~splitting_threshold ~additional_marks Span.{loc; value} =
    let f = Option.fold ~none:Flattener.empty ~some:(Flattener.singleton `Primary) loc in
    let f = List.fold_left (fun f sp -> Flattener.add `Related sp f) f additional_marks in
    {value; context = List.map contextualize_section @@ List.map (split_section ~splitting_threshold) @@ Flattener.flatten f}

  let contextualize ~splitting_threshold (d : 'code Diagnostic.t) : _ =
    R.run @@ fun () ->
    {
      code = d.code;
      severity = d.severity;
      message = contextualize_located ~splitting_threshold ~additional_marks:d.additional_marks d.message;
      backtrace = Bwd.map (contextualize_located ~splitting_threshold ~additional_marks:[]) d.backtrace;
    }
end
