module Pos =
struct
  (** The type of positions. *)
  type t =
      Pos of {
        point : int;
        (** The absolute position in the buffer, measured in graphemes clusters. *)
        bol : int;
        (** The absolute position of the beginning of the line, measured in graphemes clusters. *)
        line_num : int;
        (** The 1-indexed line number of the position. *)
        filename : string;
        (** The absolute filepath of the file that the span originated from. *)
      }

  (** Create a position pointing to the first grapheme cluster in a file. *)
  let file_start filename =
    Pos {
      point = 0;
      bol = 0;
      line_num = 1;
      filename
    }

  (** Get the 0-indexed column number of a position *)
  let col (Pos pos) =
    pos.point - pos.bol

  (** Advance a position to the next grapheme cluster. *)
  let advance (Pos pos) =
    Pos { pos with point = pos.point + 1 }

  (** Advance a position to the next line. *) let next_line (Pos pos) = Pos {
    pos with
    point = pos.point + 1;
    bol = pos.point + 1;
    line_num = pos.line_num + 1
  }
end

module Span =
struct
  (** The type of source spans. *)
  type t =
      Span of {
        start : int;
        (** The absolute starting position of the span, measured in grapheme clusters. *)
        start_bol : int;
        (** The absolute position of the beginning of the starting line, measured in grapheme clusters. *)
        start_line_num : int;
        (** The 1-indexed starting line number of the span. *)
        stop : int;
        (** The absolute stopping position of the span, measured in grapheme clusters. This is inclusive. *)
        stop_bol : int;
        (** The absolute position of the beginning of the stopping line, measured in grapheme clusters. *)
        stop_line_num : int;
        (** The 1-indexed stopping line number of the span. *)
        filename : string;
        (** The absolute filepath of the file the span originated from. *)
      }

  (** Get the 0-indexed column number of the start of the span. *)
  let start_col (Span sp) =
    sp.start - sp.start_bol

  (** Get the 0-indexed column number of the end of the span. *)
  let stop_col (Span sp) =
    sp.stop - sp.start_bol

  (** Get the width of the span in graphemes cluster. *)
  let width (Span sp) =
    sp.stop - sp.start + 1

  (** Get the starting position of a span. *)
  let start_pos (Span sp) =
    Pos.Pos {
      point = sp.start;
      bol = sp.start_bol;
      line_num = sp.start_line_num;
      filename = sp.filename
    }

  (** Get the stopping position of a span. *)
  let stop_pos (Span sp) =
    Pos.Pos {
      point = sp.stop;
      bol = sp.stop_bol;
      line_num = sp.stop_line_num;
      filename = sp.filename
    }

  let file_start filename =
    Span {
      start = 0;
      start_bol = 0;
      start_line_num = 1;
      stop = 0;
      stop_bol = 0;
      stop_line_num = 1;
      filename
    }

  (** Construct a span from 2 positions. *)
  let spanning (Pos.Pos pos_start) (Pos.Pos pos_stop) =
    Span {
      start = pos_start.point;
      start_bol = pos_start.bol;
      start_line_num = pos_start.line_num;
      stop = pos_stop.point;
      stop_bol = pos_stop.bol;
      stop_line_num = pos_stop.line_num;
      filename = pos_start.filename
    }

  (** [contains big small] will determine if the span [big] contains the span [small]. *)
  let contains (Span big) (Span small) =
    (big.start <= small.start) && (small.stop <= big.stop)

  let overlaps (Span sp0) (Span sp1) =
    (sp0.stop >= sp1.start && sp0.start <= sp1.stop) || (sp1.stop >= sp0.start && sp1.start <= sp0.stop)

  (** Construct the smallest span that contains two spans. *)
  let combine (Span sp0) (Span sp1) =
    Span {
      start = Int.min sp0.start sp1.start;
      start_bol = Int.min sp0.start_bol sp1.start_bol;
      start_line_num = Int.min sp0.start_line_num sp1.start_line_num;
      stop = Int.min sp0.stop sp1.stop;
      stop_bol = Int.min sp0.stop_bol sp1.stop_bol;
      stop_line_num = Int.min sp0.stop_line_num sp1.stop_line_num;
      filename = sp0.filename
    }

  let merge_overlapping spans =
    let merge sp0 spans = 
      match spans with
      | [] -> [sp0]
      | (sp1 :: spans) ->
        if overlaps sp0 sp1 then
          combine sp0 sp1 :: spans
        else
          sp0 :: sp1 :: spans
    in
    List.fold_right merge [] spans

  let rec differences spans =
    match spans with
    | (Span sp0 :: Span sp1 :: spans) ->
      (sp0.stop - sp1.start) :: differences (Span sp1 :: spans)
    | _ -> []

  let count_clusters str =
    Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0 str

  (** Construct a span relative to some string, using rows and columns instead of absolute positions.
      This function is rather slow, and should only be used for testing purposes. *)
  let relative str start_row start_col stop_row stop_col filename =
    let lines = String.split_on_char '\n' str in
    let start_bol =
      count_clusters @@ String.concat "\n" @@ CCList.take (start_row - 1) lines
    in
    let stop_bol =
      count_clusters @@ String.concat "\n" @@ CCList.take (stop_row - 1) lines
    in Span {
      start = start_col + start_bol;
      start_bol;
      start_line_num = start_row;
      stop = stop_col + stop_bol;
      stop_bol;
      stop_line_num = stop_row;
      filename
    }

  (** Construct a span relative to some string, using rows and columns instead of absolute positions.
      This function is rather slow, and should only be used for testing purposes. *)
  let entire str filename =
    let lines = String.split_on_char '\n' str in
    Span {
      start = 0;
      start_bol = 0;
      start_line_num = 1;
      stop = count_clusters str - 1;
      stop_bol = count_clusters @@ String.concat "\n" @@ CCList.take (List.length lines - 1) lines;
      stop_line_num = List.length lines;
      filename
    }

  let slice str (Span sp) =
    let buff = Buffer.create (sp.stop - sp.start) in
    let folder n seg =
      if sp.start <= n && n < sp.stop then
        Buffer.add_string buff seg;
      n + 1
    in
    let _ = Uuseg_string.fold_utf_8 `Grapheme_cluster folder 0 str in
    Buffer.contents buff

  let pp fmt (Span sp) =
    Format.fprintf fmt "%s@%d:%d-%d:%d"
      sp.filename
      sp.start_line_num
      (start_col (Span sp))
      sp.stop_line_num
      (stop_col (Span sp))
end

module Loc =
struct
  (** Located items. *)
  type 'a t = Loc of { span: Span.t; item : 'a }
end
