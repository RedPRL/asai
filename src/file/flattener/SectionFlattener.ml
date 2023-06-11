type t = (OrderedPosition.t * (MarkedDiagnostic.style * int)) list

let empty = []

let add (op, sp) l =
  let begin_, end_ = Asai.Span.to_positions sp in
  (begin_, (op, 1)) :: (end_, (op, -1)) :: l

open Bwd
open Bwd.Infix

(* invariant: all Asai.Span.position should have the same filename *)
type state =
  { segments : Flattened.marked_point bwd
  ; flattener_state : FlattenerState.t
  ; prev_style : MarkedDiagnostic.style option
  ; cursor : Asai.Span.position
  }

(* invariant: all Asai.Span.position should have the same filename *)
let init_state (cursor : OrderedPosition.t) =
  { segments = Emp
  ; flattener_state = FlattenerState.zero
  ; prev_style = None
  ; cursor
  }

let grouping_aux ~splitting_threshold (p : Flattened.marked_point) : Flattened.marked_point list list -> Flattened.marked_point list list =
  function
  | [] -> [[p]]
  | [] :: qss -> [p] :: qss
  | (q :: _ as qs) :: qss ->
    if q.position.line_num - p.position.line_num >= splitting_threshold &&
       p.style = None (* not highlighted or marked *)
    then
      [p] :: qs :: qss
    else
      (p :: qs) :: qss

let grouping ~splitting_threshold (input : Flattened.block) : Flattened.block list =
  List.fold_right (grouping_aux ~splitting_threshold) input []

let flatten ~splitting_threshold l =
  let module F = Flattened in
  match List.sort (fun (p1, _, _) (p2, _, _) -> OrderedPosition.compare p1 p2) l with
  | [] -> []
  | ((x, _, _) :: _) as l ->
    let loop st ((pos : Asai.Span.position), style_change, be) =
      let flattener_state = FlattenerState.apply (style_change, be) st.flattener_state in
      if st.cursor.offset = pos.offset then
        {st with flattener_state}
      else
        let current_style = FlattenerState.style st.flattener_state in
        if st.prev_style <> current_style then
          {segments = st.segments #< F.{style = current_style; position = st.cursor};
           flattener_state;
           prev_style = current_style;
           cursor = pos}
        else
          {st with flattener_state; cursor = pos}
    in
    let st = List.fold_left loop (init_state x) l in
    assert (FlattenerState.style st.flattener_state = None);
    let segments = st.segments #< F.{style = None; position = st.cursor} in
    grouping ~splitting_threshold @@ Bwd.to_list segments
