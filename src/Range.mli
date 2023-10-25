(** {1 Types} *)

(** The string source of a position or a range.

    @since 0.2.0
*)
type string_source = {
  title: string option;
  (** The title of a string source. A diagnostic handler can use the title of a string source in lieu of a file path. *)

  content: string;
  (** The content of a string source *)
}

(** The source of a position or a range. The [`String] source can be used for representing inputs in REPL.

    @since 0.2.0
*)
type source =
  [ `File of string (** A file source specified by its file path. *)
  | `String of string_source (** A string (in-memory) source. *)
  ]

(** The type of positions; this is isomorphic to {!type:Lexing.position}, but with arguably better field names. *)
type position = {
  source : source;
  (** The source (e.g., the file) that contains the position. *)

  offset : int;
  (** The 0-indexed byte offset of the position relative to the beginning of the source. *)

  start_of_line : int;
  (** The 0-indexed byte offset pointing to the start of the line that contains the position. *)

  line_num : int;
  (** The 1-indexed line number of the line that contains the position. *)
}

(** The abstract type of ranges. *)
type t

(** An auxiliary type to package data with an optional range. *)
type 'a located = { loc : t option; value : 'a }

(** {1 Ranges} *)

(** [make (beginning, ending)] builds the range [\[beginning, ending)] (not including the byte at the ending position) from a pair of positions [beginning] and [ending]. A range is empty if its beginning and ending positions are the same.

    @raise Invalid_argument if the positions do not share the same source or if [ending] comes before [beginning]. (It is okay if [ending] equals to [beginning], which means the range is empty.) The comparison of source file paths is done by [String.equal] without any path normalization.
*)
val make : position * position -> t

(** [eof pos] builds a special range referring to the end of the source. The input [pos] must be pointing at the end position; for example, if the position referring to a string source, [pos.offset] should be the length of the string. *)
val eof : position -> t

(** [view range] returns a {i view} of the range. *)
val view : t -> [`Range of position * position | `End_of_file of position]

(** [source range] returns the source associated with [range]. *)
val source : t -> source

(** [begin_line_num range] returns the 1-indexed line number of the beginning position. *)
val begin_line_num : t -> int

(** [end_line_num range] returns the 1-indexed line number of the ending position. *)
val end_line_num : t -> int

(** [begin_offset range] returns the 0-indexed offset of the (inclusive) beginning position. *)
val begin_offset : t -> int

(** [end_offset range] returns the 0-indexed offset of the (exclusive) ending position. *)
val end_offset : t -> int

(** [locate_opt r v] is [{loc = r; value = v}]. *)
val locate_opt : t option -> 'a -> 'a located

(** [locate r v] is [{loc = Some r; value = v}]. *)
val locate : t -> 'a -> 'a located

(** {1 Other Helper Functions} *)

(** [title src] gets the title of a string source or the path of a file source, or [None] if it does not exist.

    @since 0.2.0
*)
val title : source -> string option

(** {1 Support of Lexing} *)

(** [of_lex_position pos] converts an OCaml lexer position [pos] of type {!type:Lexing.position} into a {!type:position}. The input [pos] must be byte-indexed. (Therefore, the OCaml tool [ocamllex] is compatible, but the OCaml library [sedlex] is not because it uses Unicode code points.)

    @param source The source of the new position. The default source is [`File pos.pos_fname].
*)
val of_lex_position : ?source:source -> Lexing.position -> position

(** [of_lex_range (begining, ending)] takes a pair of OCaml lexer positions and creates a range. It is [make (of_lex_position begining, of_lex_position ending)].

    @param source The source of the new range. The default source is [`File begining.pos_fname].

    @raise Invalid_argument if the optional argument [source] is not given and [begining.pos_fname] and [ending.pos_fname] differ. The comparison is done by [String.equal] without any path normalization.
*)
val of_lex_range : ?source:source -> Lexing.position * Lexing.position -> t

(** [of_lexbuf lexbuf] constructs a range from the current lexeme that [lexbuf] points to. It is [of_lex_range (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)].

    @param source The source of the new range. The default source is [`File (Lexing.lexeme_start_p lexbuf).pos_fname].
*)
val of_lexbuf : ?source:source -> Lexing.lexbuf -> t

(** [locate_lex ps v] is a helper function to create a value annotated with a range. It is [locate (Some (of_lex_range ps)) v] and is designed to work with the OCaml parser generator Menhir. You can add the following code to your Menhir grammar to generate annotated data:

    {v
%inline
locate(X):
  | e = X
    { Asai.Range.locate_lex $loc e }
v}

    @param source The source of the range. The default source is [`File (Lexing.lexeme_start_p lexbuf).pos_fname].
*)
val locate_lex : ?source:source -> Lexing.position * Lexing.position -> 'a -> 'a located

(** {1 Debugging} *)

(** Ugly printer for debugging *)

val dump_source : Format.formatter -> source -> unit

val dump_position : Format.formatter -> position -> unit

val dump : Format.formatter -> t -> unit

(**/**)

(** [split range] returning the pair of the beginning and ending positions of [range].

    @raise Invalid_argument if range is a special range marking the end.
*)
val split : t -> position * position [@@ocaml.alert deprecated "Use Asai.Range.view"]
