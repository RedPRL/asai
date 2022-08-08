(** Styles *)
type style = Marked.style

(** Flatten spans in a block. *)
type block = (style option * Asai.Span.position) list

(** Flatten spans across multiple blocks. *)
type blocks = block list

type file = string * blocks
