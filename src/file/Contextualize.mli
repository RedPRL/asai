open Bwd
open Asai

(** A segment is a styled string without any control character (including new lines). *)
type segment = string Flattener.styled

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

type 'code t =
  { code : 'code (** The error code. *)
  ; severity : Severity.t (** The severity of the message. *)
  ; message : Diagnostic.message contextualized (** The marked message. *)
  ; backtrace : Diagnostic.message contextualized bwd
  }

module Make : Reader.S ->
  sig
    val contextualize : splitting_threshold:int -> 'code Asai.Diagnostic.t -> 'code t
  end
