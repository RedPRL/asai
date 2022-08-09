(** Styles *)
type style = Marked.style

(** Flattened spans in a block. *)
type block = (style option * Asai.Span.position) list

(** A collection of flattened blocks from a file. (The first component is the file path.) *)
type section = string * block list
