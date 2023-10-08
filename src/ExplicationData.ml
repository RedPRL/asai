(** A styled value is a value with a style. *)
type ('a, 'style) styled = { style : 'style; value : 'a }

(** A segment is a styled string from the user content. *)
type 'style segment = (string, 'style) styled

(** A line is a list of {!type:segment}s. *)
type 'style line = 'style segment list

(** A block is a collection of consecutive lines. *)
type 'style block =
  { start_line_num : int (** The starting 1-indexed line number of a block. *)
  ; lines : 'style line list (** The {!type:line}s within a block. *)
  }

(** A part consists of multiple blocks from the same file. These blocks should be non-overlapping and sorted by importance or the textual order. *)
type 'style part =
  { source : Span.source (** The source of a part. *)
  ; blocks : 'style block list (** The blocks within a part. *)
  }

(** Highlighted texts instead of spans. *)
type 'style t = 'style part list
