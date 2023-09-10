open Bwd

(** {1 Types} *)

(** The type of highlighting.

    @canonical Asai.Explicator.highlighting *)
type highlighting =
  [ `Primary (** The main text corresponding to the main message. *)
  | `Auxiliary (** Other texts being highlighted, which can come from additional marks or backtraces. *)
  ]

(** A convenience module for the type {!type:highlighting}.

    @canonical Asai.Explicator.Highlighting *)
module Highlighting =
struct
  (** The type of highlighting. *)
  type t = highlighting

  (** Equality testing of {!type:t}. *)
  let equal (x : t) (y : t) : bool = x = y

  (** Comparator for {!type:t}. *)
  let compare (x : t) (y : t) : int =
    match x, y with
    | `Primary, `Primary -> 0
    | `Primary, `Auxiliary -> 1
    | `Auxiliary, `Primary -> -1
    | `Auxiliary, `Auxiliary -> 0

  (** Test whether the highlighting is [`Primary]. *)
  let is_primary (x : t) = x = `Primary
end

(** A styled value is a value with an optional highlighting.

    @canonical Asai.Explicator.styled *)
type 'a styled = { style : highlighting option; value : 'a }

(** A segment is a styled string that should not have any control character (or new lines).
    
    TODO: how about tabs? 

    @canonical Asai.Explicator.segment *)
type segment = string styled

(** A line is a list of {!type:segment}s.

    @canonical Asai.Explicator.line *)
type line = segment list

(** A block is a collection of consecutive lines.

    @canonical Asai.Explicator.block *)
type block =
  { start_line_num : int (** The starting 1-indexed line number of a block. *)
  ; lines : line list (** The {!type:line}s within a block. *)
  }

(** A part consists of multiple blocks from the same file. These blocks should be non-overlapping and sorted by importance or the textual order.

    @canonical Asai.Explicator.part *)
type part =
  { file_path : string (** The file path of a part. *)
  ; blocks : block list (** The blocks within a part. *)
  }

(** An explicated value comes with highlighted texts instead of spans.

    @canonical Asai.Explicator.explicated *)
type 'a explicated = { parts : part list; value : 'a }

(** An explicated diagnostic comes with highlighted texts instead of spans.

    @canonical Asai.Explicator.diagnostic *)
type 'code diagnostic =
  { severity : Diagnostic.severity (** The severity of the message. *)
  ; code : 'code (** The message code. *)
  ; message : Diagnostic.message explicated (** The explicated message. *)
  ; backtrace : Diagnostic.message explicated bwd (** The explicated backtrace. *)
  }
