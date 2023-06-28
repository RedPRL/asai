module OrderedPosition :
sig
  type t = Asai.Span.position
  val compare : t -> t -> int
end
=
struct
  type t = Asai.Span.position
  let compare p1 p2 = Int.compare p1.Asai.Span.offset p2.Asai.Span.offset
end


module State :
sig
  type t
  val zero : t
  val style : t -> MarkedDiagnostic.style option
  val apply : MarkedDiagnostic.style * [`Begin | `End] -> t -> t
end
=
struct
  type t = { marked : int; highlighted : int }

  let zero = {marked = 0; highlighted = 0}

  let style : t -> MarkedDiagnostic.style option =
    function
    | {marked = 0; highlighted = 0} -> None
    | {marked = _; highlighted = 0} -> Some `Marked
    | {marked = _; highlighted = _} -> Some `Highlighted

  let apply (op, se) st =
    let diff = match se with `Begin -> 1 | `End -> -1 in
    match op with
    | `Marked -> {st with marked = st.marked + diff}
    | `Highlighted -> {st with highlighted = st.highlighted + diff}
end

type t = (OrderedPosition.t * (MarkedDiagnostic.style * [`Begin | `End])) list

let empty = []

let add (op, sp) l : t =
  let begin_, end_ = Asai.Span.to_positions sp in
  (begin_, (op, `Begin)) :: (end_, (op, `End)) :: l

open Bwd
open Bwd.Infix

(* invariant: all Asai.Span.position should have the same filename *)
type state =
  { segments : Flattened.marked_point bwd
  ; flattener_state : State.t
  ; prev_style : MarkedDiagnostic.style option
  ; cursor : OrderedPosition.t
  }

(* invariant: all Asai.Span.position should have the same filename *)
let init_state (cursor : OrderedPosition.t) =
  { segments = Emp
  ; flattener_state = State.zero
  ; prev_style = None
  ; cursor
  }

let grouping_aux ~splitting_threshold (p : Flattened.marked_point) :
  Flattened.marked_point list list -> Flattened.marked_point list list =
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

let flatten ~splitting_threshold (l : t) : Flattened.block list =
  let module F = Flattened in
  match List.sort (fun (p1, _) (p2, _) -> OrderedPosition.compare p1 p2) l with
  | [] -> []
  | ((x, _) :: _) as l ->
    let loop st ((pos : OrderedPosition.t), (style_change, be)) =
      let flattener_state = State.apply (style_change, be) st.flattener_state in
      if st.cursor.offset = pos.offset then
        {st with flattener_state}
      else
        let current_style = State.style st.flattener_state in
        if st.prev_style <> current_style then
          {segments = st.segments #< F.{style = current_style; position = st.cursor};
           flattener_state;
           prev_style = current_style;
           cursor = pos}
        else
          {st with flattener_state; cursor = pos}
    in
    let st = List.fold_left loop (init_state x) l in
    assert (State.style st.flattener_state = None);
    let segments = st.segments #< F.{style = None; position = st.cursor} in
    grouping ~splitting_threshold @@ Bwd.to_list segments
