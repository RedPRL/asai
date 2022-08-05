type t = { marked : int; highlighted : int }

let zero = {marked = 0; highlighted = 0}

let style : t -> MarkedText.style =
  function
  | {marked = 0; highlighted = 0} -> `Default
  | {marked = _; highlighted = 0} -> `Marked
  | {marked = _; highlighted = _} -> `Highlighted

let apply se op st =
  let diff = match se with `Start -> 1 | `End -> -1 in
  match op with
  | `Mark -> {st with marked = st.marked + diff}
  | `Highlight -> {st with highlighted = st.highlighted + diff}
