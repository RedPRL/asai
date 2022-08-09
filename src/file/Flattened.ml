(** Styles *)
type style = Marked.style

(** A point with a style. *)
type marked_point = {style : style option; position : Asai.Span.position}

(** Flattened spans in a block. *)
type block = marked_point list

(** A collection of flattened blocks from a file. (The first component is the file path.) *)
type section = string * block list
