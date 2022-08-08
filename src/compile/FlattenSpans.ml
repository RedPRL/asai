(** Styles *)
type style = MarkedText.style

(** A file consists of multiple blocks. *)
type t = (style option * Asai.Span.position) list
