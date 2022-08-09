type t = (OrderedPosition.t * Marked.style * [`Begin | `End]) list

let empty = []

let add (op, sp) l =
  let begin_, end_ = Asai.Span.to_positions sp in
  (begin_, op, `Begin) :: (end_, op, `End) :: l

open Bwd
open BwdNotation

(* invariant: all Asai.Span.position should have the same filename *)
type state =
  { segments : (Marked.style option * Asai.Span.position) bwd
  ; flatter_state : FlatterState.t
  ; prev_style : Marked.style option
  ; cursor : Asai.Span.position
  }

(* invariant: all Asai.Span.position should have the same filename *)
let init_state (cursor : OrderedPosition.t) =
  { segments = Emp
  ; flatter_state = FlatterState.zero
  ; prev_style = None
  ; cursor
  }

let grouping ~splitting_threshold : Flattened.block -> Flattened.blocks =
  let rec loop =
    let open Asai.Span in
    function
    | [] -> assert false
    | [p] -> p, [], []
    | p :: ps ->
      let q, qs, qss = loop ps in
      if (snd q).line_num - (snd p).line_num >= splitting_threshold &&
         (fst p) = None (* not highlighted or marked *)
      then
        p, [], ((q::qs) :: qss)
      else
        p, (q::qs), qss
  in
  function
  | [] -> []
  | ps ->
    let p, ps, pss = loop ps in
    (p :: ps) :: pss

let flatten ~splitting_threshold l =
  match List.sort (fun (p1, _, _) (p2, _, _) -> OrderedPosition.compare p1 p2) l with
  | [] -> []
  | ((x, _, _) :: _) as l ->
    let loop st ((pos : Asai.Span.position), style_change, be) =
      let flatter_state = FlatterState.apply (style_change, be) st.flatter_state in
      if st.cursor.offset = pos.offset then
        {st with flatter_state}
      else
        let current_style = FlatterState.style st.flatter_state in
        if st.prev_style <> current_style then
          {segments = st.segments #< (current_style, st.cursor);
           flatter_state;
           prev_style = current_style;
           cursor = pos}
        else
          {st with flatter_state; cursor = pos}
    in
    let st = List.fold_left loop (init_state x) l in
    assert (FlatterState.style st.flatter_state = None);
    let segments = st.segments #< (None, st.cursor) in
    grouping ~splitting_threshold @@ Bwd.to_list segments