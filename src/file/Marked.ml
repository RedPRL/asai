open Bwd

(** Styles *)
type style = [`Highlighted | `Marked]

(** A segment is a styled string without control characters. *)
type segment = style option * string

(** A line is a list of segments. *)
type line = segment list

(** A block is a collection of consecutive lines. *)
type block = { start_line_num : int; text : line list }

type blocks = block list

(** A file consists of multiple blocks. *)
type file = { file_path : string; blocks : block list }

(** A multi-span consists of all formatted spans across multiple files. *)
type message = file list * Asai.Diagnostic.message

(** a message *)
type t =
  { code : string
  ; severity : Asai.Severity.t
  ; message : message
  ; traces : message bwd
  }
