open Bwd
open Bwd.Infix

open Explication
include ExplicatorSigs

(* used by the register_printer below *)
let blame_range mode fmt r =
  let read = SourceReader.unsafe_get (SourceReader.load (Range.source r)) in
  let line_num p = UserContent.count_newlines ~line_breaks:mode read (0, p.Range.offset) + 1 in
  match Range.view r with
  | `End_of_file p ->
    Format.fprintf fmt
      "@[@[%a@]@ should probably have line number %i.@]"
      Range.dump r (line_num p)
  | `Range (p1, p2) ->
    Format.fprintf fmt
      "@[@[%a@]@ should probably have line numbers %i and %i.@]"
      Range.dump r (line_num p1) (line_num p2)

let () = Printexc.register_printer @@
  function
  | Unexpected_end_of_source _pos ->
    Some "Asai.Explicator.Unexpected_end_of_source; turn on the debug mode (e.g., Term.display ~debug:true) and check your lexer"
  | Unexpected_line_num_increment _pos ->
    Some "Asai.Explicator.Unexpected_line_num_increment; turn on the debug mode (e.g., Term.display ~debug:true) and check your lexer"
  | Unexpected_newline _pos ->
    Some "Asai.Explicator.Unexpected_newline; turn on the debug mode (e.g., Term.display ~debug:true) and check your lexer"
  | Unexpected_position_in_newline _pos ->
    Some "Asai.Explicator.Unexpected_newline; turn on the debug mode (e.g., Term.display ~debug:true) and check your lexer"
  | Invalid_ranges (mode, rs) ->
    Option.some begin
      SourceReader.run @@ fun () ->
      Format.asprintf "@[<2>These ranges have incorrect line numbers:@ %a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_force_newline (blame_range mode)) rs
    end
  | _ -> None

let to_start_of_line (pos : Range.position) = {pos with offset = pos.start_of_line}
let default_blend ~(priority : _ -> int) t1 t2 = if priority t2 <= priority t1 then t2 else t1

module Make (Tag : Tag) = struct
  type position = Range.position

  (** Skip the newline sequence, assuming that [shift] is not zero. (Otherwise, it means we already reached eof.) *)
  let eol_to_next_line shift (pos : position) : position =
    assert (shift <> 0);
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
    ; cursor : Range.position
    ; eol : int
    ; eol_shift : int option
    ; line_num : int
    }

  module F = Flattener.Make(Tag)

  let explicate_block ~line_breaks (b : Tag.t Flattener.block) : Tag.t block =
    match b.tagged_positions with
    | [] -> invalid_arg "explicate_block: empty block"
    | ((_, ploc) :: _) as ps ->
      let source = SourceReader.load ploc.source in
      let eof = SourceReader.length source in
      let find_eol i = UserContent.find_eol ~line_breaks (SourceReader.unsafe_get source) (i, eof) in
      let[@tailcall] rec go state : (Tag.t option * Range.position) list -> _ =
        function
        | (ptag, ploc) :: ps when state.cursor.line_num = ploc.line_num ->
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
                state.segments
                <: (state.current_tag, read_between ~source state.cursor.offset state.eol)
              else if Option.is_none state.eol_shift && Option.is_some state.current_tag then
                state.segments
                <: (state.current_tag, "‹EOF›")
              else
                state.segments
            in
            let tagged_lines, remaining_tagged_lines = Utils.span (fun (_, i) -> i = state.line_num) state.remaining_tagged_lines in
            (state.lines <: {segments = Bwd.to_list segments; tags = List.map fst tagged_lines}), remaining_tagged_lines
          in
          (* Continue the process if [ps] is not empty. *)
          match ps, state.eol_shift with
          | [], _ ->
            assert (state.line_num = b.end_line_num);
            lines
          | (_, ploc) :: _, None -> raise @@ Unexpected_end_of_source ploc
          | (_, ploc) :: _, Some eol_shift ->
            if ploc.offset > eof then raise @@ Unexpected_end_of_source ploc;
            if ploc.offset <= state.eol then raise @@ Unexpected_line_num_increment ploc;
            if ploc.offset < state.eol + eol_shift then raise @@ Unexpected_position_in_newline ploc;
            (* Okay, p is really on the next line *)
            let cursor = eol_to_next_line eol_shift {state.cursor with offset = state.eol} in
            let eol, eol_shift = find_eol (state.eol + eol_shift) in
            go
              { lines
              ; segments = Emp
              ; remaining_tagged_lines
              ; current_tag = state.current_tag
              ; cursor
              ; eol
              ; eol_shift
              ; line_num = state.line_num + 1
              }
              ps
      in
      let begin_pos = to_start_of_line ploc in
      let eol, eol_shift = find_eol ploc.offset in
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
      { begin_line_num = b.begin_line_num
      ; end_line_num = b.end_line_num
      ; lines = Bwd.to_list @@ lines
      }

  let[@inline] explicate_blocks ~line_breaks = List.map (explicate_block ~line_breaks)

  let[@inline] explicate_part ~line_breaks (source, bs) : Tag.t part =
    { source; blocks = explicate_blocks ~line_breaks bs }

  let check_ranges ~line_breaks ranges =
    let broken_ranges =
      List.filter_map
        (fun (_, r) ->
           let read = SourceReader.unsafe_get @@ SourceReader.load @@ Range.source r in
           if not @@ UserContent.check_line_num ~line_breaks read r then Some r else None)
        ranges
    in
    if broken_ranges <> [] then raise @@ Invalid_ranges (line_breaks, broken_ranges)

  let explicate ?(line_breaks=`Traditional) ?(block_splitting_threshold=5)
      ?(blend=default_blend ~priority:Tag.priority) ?(debug=false) ranges =
    if debug then check_ranges ~line_breaks ranges;
    List.map (explicate_part ~line_breaks) @@ F.flatten ~block_splitting_threshold ~blend ranges
end
