open Bwd

(** Styles *)
type style = [`Highlighted | `Marked]

(** A segment is a styled string without control characters. *)
type segment = style option * string

(** A line is a list of segments. *)
type line = segment list

(** A block is a collection of consecutive lines. *)
type block = { start_line_num : int; lines : line list }

(** A section consists of multiple blocks from a file. *)
type section = { file_path : string; blocks : block list }

(** A multi-span consists of all formatted spans across multiple files. *)
type message = section list * Asai.Diagnostic.message

(** a message *)
type 'code t =
  { code : 'code
  ; severity : Asai.Severity.t
  ; message : message
  ; backtrace : message bwd
  }
