open Bwd
open Bwd.Infix

open Explication
include ExplicatorSigs

let to_start_of_line (pos : Span.position) = {pos with offset = pos.start_of_line}

module Make (Tag : Tag) = struct
  type position = Span.position

  (** [find_eol_traditional pos] finds the position of the next ['\n']. If the end of source is reached before ['\n'], then the position of the end of the source is returned. *)
  let find_eol_traditional ~source ~eof next =
    let rec go i =
      if i >= eof then
        eof, 0
      else
        match SourceReader.unsafe_get source i with
        | '\n' -> i, 1 (* LF *)
        | '\r' ->
          if i+1 < eof && SourceReader.unsafe_get source (i+1) = '\n'
          then i, 2 (* CRLF *)
          else i, 1 (* CR *)
        | _ ->
          go (i+1)
    in
    go next

  (** [find_eol_unicode pos] finds the position of the next ['\n']. If the end of source is reached before ['\n'], then the position of the end of the source is returned. *)
  let find_eol_unicode ~source ~eof next =
    let rec go i =
      if i >= eof then
        eof, 0
      else
        match SourceReader.unsafe_get source i with
        | '\n' (* LF *) | '\x0b' (* VT *) | '\x0c' (* FF *) -> i, 1
        | '\r' ->
          if i+1 < eof && SourceReader.unsafe_get source (i+1) = '\n'
          then i, 2 (* CRLF *)
          else i, 1 (* CR *)
        | '\xc2' ->
          if i+1 < eof && SourceReader.unsafe_get source (i+1) = '\x85'
          then i, 2 (* NEL *)
          else go (i+1)
        | '\xe2' ->
          if i+2 < eof && SourceReader.unsafe_get source (i+1) = '\x80' &&
             (let c2 = SourceReader.unsafe_get source (i+2) in c2 = '\xa8' || c2 = '\xa9')
          then i, 3 (* LS and PS *)
          else go (i+1)
        | _ ->
          go (i+1)
    in
    go next

  (** Skip the ['\n'] character, assuming that [eol] is not the end of source *)
  let eol_to_next_line shift (pos : position) : position =
    { source = pos.source;
      (* Need to update our offset to skip the newline char *)
      offset = pos.offset + shift;
      start_of_line = pos.offset + shift;
      line_num = pos.line_num + 1 }

  let read_between ~source begin_ end_ : string =
    String.init (end_ - begin_) @@ fun i ->
    SourceReader.unsafe_get source (begin_ + i)

  type explicator_state =
    { lines : Tag.t line bwd
    ; segments : Tag.t segment bwd
    ; remaining_tagged_lines : (Tag.t * int) list
    ; current_tag : Tag.t option
    ; cursor : Span.position
    ; eol : int
    ; eol_shift : int
    ; line_num : int
    }

  exception Unexpected_end_of_source of Span.position
  exception Unexpected_line_num_increment of Span.position
  exception Unexpected_newline of Span.position
  exception Unexpected_position_in_newline of Span.position

  module F = Flattener.Make(Tag)

  let explicate_block ~line_breaking (b : F.block) : Tag.t block =
    let find_eol = match line_breaking with `Unicode -> find_eol_unicode | `Traditional -> find_eol_traditional in
    match b.tagged_positions with
    | [] -> invalid_arg "explicate_block: empty block"
    | ((_, ploc) :: _) as ps ->
      let source = SourceReader.load ploc.source in
      let eof = SourceReader.length source in
      let[@tailcall] rec go state : (Tag.t option * Span.position) list -> _ =
        function
        | (ptag,ploc)::ps when state.cursor.line_num = ploc.line_num ->
          if ploc.offset > eof then raise @@ Unexpected_end_of_source ploc;
          if ploc.offset > state.eol then raise @@ Unexpected_newline ploc;
          if ploc.offset = state.cursor.offset then
            go {state with cursor = ploc; current_tag = ptag} ps
          else
            (* Still on the same line *)
            let segments =
              state.segments <:
              (state.current_tag, read_between ~source state.cursor.offset ploc.offset)
            in
            go { state with segments; cursor = ploc; current_tag = ptag } ps
        | ps ->
          (* Shifting to the next line *)
          let lines, remaining_tagged_lines =
            let segments =
              if state.cursor.offset < state.eol then
                state.segments <:
                (state.current_tag, read_between ~source state.cursor.offset state.eol)
              else
                state.segments
            in
            let tagged_lines, remaining_tagged_lines = Utils.span (fun (_, i) -> i = state.line_num) state.remaining_tagged_lines in
            (state.lines <: {segments = Bwd.to_list segments; tags = List.map fst tagged_lines}), remaining_tagged_lines
          in
          (* Continue the process if [ps] is not empty. *)
          match ps with
          | [] ->
            assert (Option.is_none state.current_tag); lines
          | (_, ploc) :: _ ->
            if ploc.offset > eof then raise @@ Unexpected_end_of_source ploc;
            if ploc.offset <= state.eol then raise @@ Unexpected_line_num_increment ploc;
            if ploc.offset < state.eol + state.eol_shift then raise @@ Unexpected_position_in_newline ploc;
            (* Okay, p is really on the next line *)
            let cursor =
              eol_to_next_line state.eol_shift {state.cursor with offset = state.eol}
            in
            let eol, eol_shift = find_eol ~source ~eof (state.eol + state.eol_shift) in
            go {lines; segments=Emp; current_tag = state.current_tag; cursor; eol; eol_shift; remaining_tagged_lines; line_num = state.line_num + 1} ps
      in
      let begin_pos = to_start_of_line ploc in
      let eol, eol_shift = find_eol ~source ~eof ploc.offset in
      let lines =
        go
          { lines = Emp
          ; segments = Emp
          ; remaining_tagged_lines = b.tagged_lines
          ; current_tag = None
          ; cursor = begin_pos
          ; eol
          ; eol_shift
          ; line_num = b.begin_line_num
          }
          ps
      in
      { begin_line_num = b.end_line_num
      ; end_line_num = b.end_line_num
      ; lines = Bwd.to_list @@ lines
      }

  let[@inline] explicate_blocks ~line_breaking = List.map (explicate_block ~line_breaking)

  let[@inline] explicate_part ~line_breaking (source, bs) : Tag.t part =
    { source; blocks = explicate_blocks ~line_breaking bs }

  let default_blend t1 t2 =
    if Tag.priority t2 <= Tag.priority t1 then t2 else t1

  let explicate ?(line_breaking=`Traditional) ?(block_splitting_threshold=0) ?(blend=default_blend) spans =
    List.map (explicate_part ~line_breaking) @@ F.flatten ~block_splitting_threshold ~blend spans
end
