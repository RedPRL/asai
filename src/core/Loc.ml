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
      counts for spans, instead returning raw byte indices (ocamllex),
      or codepoint indices (sedlex).

   Codepoint indexing is also out, as slicing/indexing are O(n) operations,
   which is a total dealbreaker.

   Instead, we measure everything in bytes (or, in fancier terms, UTF-8 codeunits).
   This gives us O(1) indexing + slicing, which is really what we wanted all along.
*)

(** Count the number of codepoints until a newline or the end of the string. *)
let bytes_till_newline str start =
  let rec go n ix =
    if ix >= String.length str then
      n
    else
      let decoded = String.get_utf_8_uchar str n in
      let uch = Uchar.utf_decode_uchar decoded in
      let nbytes = Uchar.utf_decode_length decoded in
      if Uchar.equal uch (Uchar.of_char '\n') then
        n
      else
        go (n + nbytes) (ix + nbytes)
  in
  go 0 start

module Pos =
struct
  (* See [NOTE: Bytes, Codepoints, and Grapheme Clusters] *)
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

  let create ~point ~bol ~line ~filename = { 
    point;
    bol;
    line;
    filename
  }

  let of_lex_pos (pos : Lexing.position) = {
    point = pos.pos_cnum;
    bol = pos.pos_bol;
    line = pos.pos_lnum;
    filename = pos.pos_fname;
  }

  let filename pos = pos.filename
  let offset pos = pos.point
  let line pos = pos.line

  let utf8_slice_line str pos =
    String.sub str pos.bol (bytes_till_newline str pos.bol)
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

  let of_lex_pos (pos_start : Lexing.position) (pos_stop : Lexing.position) =
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

  let file_start filename = {
    start = 0;
    start_bol = 0;
    start_line = 1;
    stop = 0;
    stop_bol = 0;
    stop_line = 1;
    filename;
  }

  let filename sp = sp.filename
  let start_line sp = sp.start_line
  let stop_line sp = sp.start_line

  let height sp =
    sp.stop_line - sp.start_line + 1

  let line_numbers sp =
    List.init (height sp) (fun ix -> sp.start_line + ix)


  let utf8_slice str sp =
    String.sub str sp.start (sp.stop - sp.start)

  let utf8_slice_lines str sp =
    let before = String.sub str sp.start_bol (sp.start - sp.start_bol) in
    let middle = utf8_slice str sp in
    let after = String.sub str sp.stop (bytes_till_newline str sp.stop) in
    (before, middle, after)

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

  let spanning (start_pos : Pos.t) (stop_pos : Pos.t) =
    if (start_pos.filename <> stop_pos.filename) then
      let msg =
        Format.asprintf "The filenames %s and %s did not match"
          start_pos.filename
          stop_pos.filename
      in
      raise @@ Invalid_argument msg
    else {
      start = start_pos.point;
      start_bol = start_pos.bol;
      start_line = start_pos.line;
      stop = stop_pos.point;
      stop_bol = stop_pos.bol;
      stop_line = stop_pos.line;
      filename = start_pos.filename
    }
end

module Loc =
struct
  type 'a t = { span : Span.t; value : 'a }

  let value loc = loc.value
end
