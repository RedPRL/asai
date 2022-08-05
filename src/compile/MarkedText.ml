open Bwd

(** Styles *)
type style = [`Context | `Highlight | `Mark]

(** A segment is a styled string without control characters. *)
type marked_string = style * string

(** A line is a list of segments. *)
type marked_line = marked_string list

(** A block is a collection of consecutive lines. *)
type marked_block = { start_line_num : int; text : marked_line list }

(** A file consists of multiple blocks. *)
type marked_file = { file_path : string; blocks : marked_block list }

(** A multi-span consists of all formatted spans across multiple files. *)
type marked_message = marked_file list * Asai.Diagnostic.message

(** a message *)
type t =
  { code : string
  ; severity : Asai.Severity.t
  ; message : marked_message
  ; traces : marked_file bwd
  }
