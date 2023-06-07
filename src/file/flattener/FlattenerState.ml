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
