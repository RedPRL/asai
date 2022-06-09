(* [NOTE: Codepoints, Codeunits, and Grapheme Clusters]
   In general, when doing any sort of user-facing display of things
   like column numbers, we want to count (Extended) Grapheme Clusters
   as defined in UAX#29. These are what we could best call a single "character",
   at least for the purposes of text display. As an example, the "character"
   'ä' really consists of 2 codepoints: one for the letter 'a', and another
   for the diacritic.

   This may make it seem like we want to use grapheme clusters everywhere in
   our span, but this isn't feasible for 2 reasons.
   1. Slicing becomes an O(n) operation, as we need to perform cluster
      counting during the slice.
   2. As a more practical matter, none of the OCaml lexers return cluster
      counts for spans, instead returning raw byte indicies (ocamllex),
      or codepoint indicies (sedlex).

   Codepoint indexing is also out, as slicing/indexing are O(n) operations,
   which is a total dealbreaker.

   Instead, we measure everything in bytes (or, in fancier terms, UTF-8 codeunits).
   This gives us O(1) indexing + slicing, which is really what we wanted all along.
*)

let rec find_end_of_line n str =
  let decoded = String.get_utf_8_uchar str n in
  if Uchar.equal (Uchar.utf_decode_uchar decoded) (Uchar.of_char '\n') then
    n
  else
    find_end_of_line (n + Uchar.utf_decode_length decoded) str

module Pos =
struct
  type t = {
    point : int;
    (** The byte index of the position. *)
    bol : int;
    (** A byte-index pointing to the beginning of the line that contains position. *)
    line : int;
    (** The 1-indexed line number of the start of the position. *)
    filename : string
    (** The absolute file name of the file that contains the position. *)
  }

  let filename pos = pos.filename
  let line pos = pos.line

  let utf8_slice_line str pos =
    String.sub str pos.bol (find_end_of_line pos.point str)
end

module Span =
struct
  (* See [NOTE: Bytes, Codepoints, and Grapheme Clusters] *)
  type t = {
    start : int;
    (** A byte-index pointing to the start of the source span. *)
    start_bol : int;
    (** A byte-index pointing to the beginning of the line that contains the start of the source span. *)
    start_line : int;
    (** The 1-indexed line number of the start of the span. *)
    stop : int;
    (** A byte-index pointing to the end of the source span. *)
    stop_bol : int;
    (** A byte-index pointing to the beginning of the line that contains the end of the source span. *)
    stop_line : int;
    (** The 1-indexed line number of the stop of the span. *)
    filename : string
    (** The absolute file name of the file that contains the span. *)
  }

  let create (pos_start : Lexing.position) (pos_stop : Lexing.position) =
    if (pos_start.pos_fname <> pos_stop.pos_fname) then
      let msg =
        Format.asprintf "The filenames %s and %s did not match"
          pos_start.pos_fname
          pos_stop.pos_fname
      in
      raise @@ Invalid_argument msg
    else {
      start = pos_start.pos_cnum;
      start_bol = pos_start.pos_bol;
      start_line = pos_start.pos_lnum;
      stop = pos_stop.pos_cnum;
      stop_bol = pos_stop.pos_bol;
      stop_line = pos_stop.pos_lnum;
      filename = pos_start.pos_fname
    }

  let filename sp = sp.filename
  let start_line sp = sp.start_line
  let stop_line sp = sp.start_line

  let utf8_slice str sp =
    String.sub str sp.start sp.stop

  let utf8_slice_lines str sp =
    String.sub str sp.start_bol (find_end_of_line sp.stop str)

  let start_pos sp : Pos.t = {
    point = sp.start;
    bol = sp.start_bol;
    line = sp.start_line;
    filename = sp.filename
  }

  let stop_pos sp : Pos.t = {
    point = sp.stop;
    bol = sp.stop_bol;
    line = sp.stop_line;
    filename = sp.filename
  }
end

module Loc =
struct
  type 'a t = { span : Span.t; value : 'a }

  let value loc = loc.value
end
