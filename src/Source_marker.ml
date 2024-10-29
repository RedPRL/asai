open Bwd
open Bwd.Infix

open Marked_source
include Source_marker_sigs

(* helper functions used by the register_printer below *)

let print_invalid_offset fmt : String_utils.invalid_offset -> unit =
  function
  | `Negative i ->
    Format.fprintf fmt "its@ offset@ %d@ is@ negative." i
  | `Beyond_end_of_file (i, e) ->
    Format.fprintf fmt "its@ offset@ %d@ is@ beyond@ the@ end@ of@ file@ (%d)." i e
  | `Within_newline (i, (s, e)) ->
    Format.fprintf fmt "its@ offset@ %d@ is@ within@ a@ newline@ sequence@ [%d,%d)." i s e

let print_invalid_position fmt : String_utils.invalid_position -> unit =
  function
  | `Offset r ->
    print_invalid_offset fmt r
  | `Incorrect_start_of_line (s, s') ->
    Format.fprintf fmt "its@ start@ of@ line@ is@ %d@ but@ it@ should@ have@ been@ %d." s s'
  | `Incorrect_line_num (ln, ln') ->
    Format.fprintf fmt "its@ line@ number@ is@ %d@ but@ it@ should@ have@ been@ %d." ln ln'

let print_invalid_range fmt : String_utils.invalid_range -> unit =
  function
  | `Begin r ->
    Format.fprintf fmt "its@ beginning@ position@ is@ invalid;@ %a" print_invalid_position r
  | `End r ->
    Format.fprintf fmt "its@ ending@ position@ is@ invalid;@ %a" print_invalid_position r

let () = Printexc.register_printer @@
  function
  | Invalid_range (range, reason) ->
    Some begin
      Source_reader.run @@ fun () ->
      Format.asprintf "@[<2>Invalid range:@ @[%a@]@ is@ invalid@ because@ %a"
        Range.dump range print_invalid_range reason
    end
  | _ -> None

let to_start_of_line (pos : Range.position) = {pos with offset = pos.start_of_line}

module Make (Tag : Tag) = struct
  type position = Range.position

  (** Skip the newline sequence, assuming that [shift] is not zero. (Otherwise, it means we already reached EOF.) *)
  let eol_to_next_line shift (pos : position) : position =
    assert (shift <> 0);
    { source = pos.source;
      (* Need to update our offset to skip the newline char *)
      offset = pos.offset + shift;
      start_of_line = pos.offset + shift;
      line_num = pos.line_num + 1 }

  let read_between ~source begin_ end_ : string =
    String.init (end_ - begin_) @@ fun i ->
    Source_reader.unsafe_get source (begin_ + i)

  type marker_state =
    { lines : Tag.t line bwd
    ; tokens : Tag.t token bwd
    ; remaining_line_marks : (int * Tag.t) list
    ; cursor : Range.position
    ; eol : int
    ; eol_shift : int option
    ; line_num : int
    }

  module F = Range_flattener.Make(Tag)

  let mark_block ~line_breaks source (b : Tag.t Range_flattener.block) : Tag.t block =
    match b.marks with
    | [] -> invalid_arg "mark_block: empty block; should be impossible"
    | ((first_loc, _) :: _) as marks ->
      let source = Source_reader.load source in
      let eof = Source_reader.length source in
      let find_eol i = String_utils.find_eol ~line_breaks (Source_reader.unsafe_get source) (i, eof) in
      let rec go state : (Range.position * Tag.t mark) list -> _ =
        function
        | (loc, mark) :: marks when state.cursor.line_num = loc.line_num (* on the same line *) ->
          if loc.offset > eof then invalid_arg "Asai.Source_marker.mark: position beyond EOF; use the debug mode";
          if loc.offset > state.eol then invalid_arg "Asai.Source_marker.mark: unexpected newline; use the debug mode";
          let special_position =
            if loc.offset = state.eol then
              if loc.offset = eof then
                Some `End_of_file
              else
                Some `End_of_line
            else
              None
          in
          let tokens =
            if loc.offset = state.cursor.offset then
              state.tokens <: Mark (special_position, mark)
            else
              state.tokens <: String (read_between ~source state.cursor.offset loc.offset) <: Mark (special_position, mark)
          in
          go { state with tokens; cursor = loc } marks
        | marks ->
          (* Shifting to the next line *)
          let lines, remaining_line_marks =
            let tokens =
              if state.cursor.offset < state.eol then
                state.tokens <: String (read_between ~source state.cursor.offset state.eol)
              else
                state.tokens
            in
            let line_marks, remaining_line_marks =
              Utils.span (fun (line_num, _) -> line_num = state.line_num) state.remaining_line_marks
            in
            (state.lines <:
             { tokens = Bwd.to_list tokens
             ; marks = List.map snd line_marks
             }),
            remaining_line_marks
          in
          (* Continue the process if [marks] is not empty. *)
          match marks, state.eol_shift with
          | [], _ ->
            assert (state.line_num = b.end_line_num);
            lines
          | _ :: _, None -> invalid_arg "Asai.Source_marker.mark: position beyond EOF; use the debug mode"
          | (loc, _) :: _, Some eol_shift ->
            if loc.offset > eof then invalid_arg "Asai.Source_marker.mark: position beyond EOF; use the debug mode";
            if loc.offset <= state.eol then invalid_arg "Asai.Source_marker.mark: expected newline missing; use the debug mode";
            if loc.offset < state.eol + eol_shift then invalid_arg "Asai.Source_marker.mark: offset within newline; use the debug mode";
            (* Okay, p is really on the next line *)
            let cursor = eol_to_next_line eol_shift {state.cursor with offset = state.eol} in
            let eol, eol_shift = find_eol (state.eol + eol_shift) in
            go
              { lines
              ; tokens = Emp
              ; remaining_line_marks
              ; cursor
              ; eol
              ; eol_shift
              ; line_num = state.line_num + 1
              }
              marks
      in
      let lines =
        let begin_pos = to_start_of_line first_loc in
        let eol, eol_shift = find_eol first_loc.offset in
        go
          { lines = Emp
          ; tokens = Emp
          ; remaining_line_marks = b.line_marks
          ; cursor = begin_pos
          ; eol
          ; eol_shift
          ; line_num = b.begin_line_num
          }
          marks
      in
      { begin_line_num = b.begin_line_num
      ; end_line_num = b.end_line_num
      ; lines = Bwd.to_list @@ lines
      }

  let[@inline] mark_blocks ~line_breaks source ranges =
    List.map (mark_block ~line_breaks source) ranges

  let[@inline] mark_part ~line_breaks (source, bs) : Tag.t part =
    { source; blocks = mark_blocks ~line_breaks source bs }

  let check_ranges ~line_breaks ranges =
    List.iter
      (fun (range, _) ->
         let source = Source_reader.load @@ Range.source range in
         let read = Source_reader.unsafe_get source in
         let eof = Source_reader.length source in
         try String_utils.check_range ~line_breaks ~eof read range
         with String_utils.Invalid_range reason -> raise @@ Invalid_range (range, reason))
      ranges

  let mark ?(line_breaks=`Traditional) ?(block_splitting_threshold=5) ?(debug=false) ranges =
    if debug then check_ranges ~line_breaks ranges;
    List.map (mark_part ~line_breaks) @@ F.flatten ~block_splitting_threshold ranges
end
