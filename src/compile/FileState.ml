type t = (OrderedPosition.t * MarkedText.style * [`Begin | `End]) list

let empty = []

let add (op, sp) l =
  let begin_, end_ = Asai.Span.to_positions sp in
  (begin_, op, `Begin) :: (end_, op, `End) :: l

open Bwd
open BwdNotation

(* invariant: all Asai.Span.position should have the same filename *)
type state =
  { segments : (MarkedText.style option * Asai.Span.position) bwd
  ; scanner_state : ScannerState.t
  ; prev_style : MarkedText.style option
  ; cursor : Asai.Span.position
  }

(* invariant: all Asai.Span.position should have the same filename *)
let init_state (cursor : OrderedPosition.t) =
  { segments = Emp
  ; scanner_state = ScannerState.zero
  ; prev_style = None
  ; cursor
  }

let flatten l =
  match List.sort (fun (p1, _, _) (p2, _, _) -> OrderedPosition.compare p1 p2) l with
  | [] -> []
  | ((x, _, _) :: _) as l ->
    let loop st ((pos : Asai.Span.position), style_change, be) =
      let scanner_state = ScannerState.apply (style_change, be) st.scanner_state in
      if st.cursor.offset = pos.offset then
        {st with scanner_state}
      else
        let current_style = ScannerState.style st.scanner_state in
        if st.prev_style <> current_style then
          {segments = st.segments #< (current_style, st.cursor);
           scanner_state;
           prev_style = current_style;
           cursor = pos}
        else
          {st with scanner_state; cursor = pos}
    in
    Bwd.to_list (List.fold_left loop (init_state x) l).segments
