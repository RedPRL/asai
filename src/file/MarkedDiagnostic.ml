open Bwd

(** Styles *)
type style = [`Highlighted | `Marked]

(** A segment is a styled string without any control character (including new lines). *)
type segment = style option * string

(** A line is a list of segments. *)
type line = segment list

(** A block is a collection of consecutive lines. *)
type block =
  { start_line_num : int (** The starting line number of a block. *)
  ; lines : line list (** The lines within a block. *)
  }

(** A section consists of multiple blocks from the same file. In practice, these blocks should be non-overlapping and sorted by importance or the textual order. *)
type section =
  { file_path : string (** The file path of a section. *)
  ; blocks : block list (** The blocks within a block. *)
  }

(** A multi-span is a diagnostic with all formatted spans across multiple files. *)
type message = section list * Asai.Diagnostic.message

(** A diagnostic rendered with actual (formatted) text. *)
type 'code t =
  { code : 'code (** The error code. *)
  ; severity : Asai.Severity.t (** The severity of the message. *)
  ; message : message (** The marked message. *)
  ; backtrace : message bwd
  }
