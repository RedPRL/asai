(** {1 Types} *)

(** A styled value is a value with a style.

      @canonical Asai.Explicator.styled *)
type ('a, 'style) styled = { style : 'style; value : 'a }

(** A segment is a styled string that should not have any control character (or new lines).

    TODO: how about tabs?

    @canonical Asai.Explicator.segment *)
type 'style segment = (string, 'style) styled

(** A line is a list of {!type:segment}s.

    @canonical Asai.Explicator.line *)
type 'style line = 'style segment list

(** A block is a collection of consecutive lines.

    @canonical Asai.Explicator.block *)
type 'style block =
  { start_line_num : int (** The starting 1-indexed line number of a block. *)
  ; lines : 'style line list (** The {!type:line}s within a block. *)
  }

(** A part consists of multiple blocks from the same file. These blocks should be non-overlapping and sorted by importance or the textual order.

    @canonical Asai.Explicator.part *)
type 'style part =
  { file_path : string (** The file path of a part. *)
  ; blocks : 'style block list (** The blocks within a part. *)
  }

(** Highlighted texts instead of spans.

    @canonical Asai.Explicator.explication *)
type 'style explication = 'style part list
