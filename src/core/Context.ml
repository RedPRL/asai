open Bwd

module Highlighting =
struct
  type t = [`Primary | `Related]
  let equal (x : t) (y : t) : bool = x = y

  let compare (x : t) (y : t) : int =
    match x, y with
    | `Primary, `Primary -> 0
    | `Primary, `Related -> 1
    | `Related, `Primary -> -1
    | `Related, `Related -> 0

  let is_primary t = t = `Primary
end

type highlighting = Highlighting.t
type 'a styled = { style : highlighting option; value : 'a }

(** A segment is a styled string without any control character (including new lines). *)
type segment = string styled

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

type 'a contextualized =
  { value : 'a
  ; context : section list
  }

type 'code diagnostic =
  { code : 'code (** The error code. *)
  ; severity : Severity.t (** The severity of the message. *)
  ; message : Diagnostic.message contextualized (** The marked message. *)
  ; backtrace : Diagnostic.message contextualized bwd
  }
