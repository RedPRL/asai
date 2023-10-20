(** {1 Types} *)

(** The string source of a position or a span. *)
type string_source = {
  title: string option;
  (** The title of a string source. A diagnostic handler can use the title of a string source in lieu of a file path. *)

  content: string;
  (** The content of a string source *)
}

(** The source of a position or a span. The [`String] source can be used for REPL. *)
type source =
  [ `File of string (** File path of the source file. *)
  | `String of string_source (** The content of an in-memory source. *)
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

(** The abstract type of spans. *)
type t

(** An auxiliary type to package data with an optional span. *)
type 'a located = { loc : t option; value : 'a }

(** {1 Spans} *)

(** [make (beginning, ending)] builds the span [\[beginning, ending)] (not including the byte at the ending position) from a pair of positions [beginning] and [ending].

    @raise Invalid_argument if the positions do not share the same source or if [ending] comes before [beginning]. (It is okay if [ending] equals to [beginning], which means the span is empty.) The comparison of source file paths is done by [String.equal] without any path normalization.
*)
val make : position * position -> t

(** [split span] returning the pair of the beginning and ending positions of [span]. It is the right inverse of {!val:make}. *)
val split : t -> position * position

(** [source span] returns the source associated with [span]. *)
val source : t -> source

(** [begin_line_num span] returns the 1-indexed line number of the beginning position. *)
val begin_line_num : t -> int

(** [end_line_num span] returns the 1-indexed line number of the ending position. *)
val end_line_num : t -> int

(** [begin_offset span] returns the 0-indexed offset of the (inclusive) beginning position. *)
val begin_offset : t -> int

(** [end_offset span] returns the 0-indexed offset of the (exclusive) ending position. *)
val end_offset : t -> int

(** [locate_opt sp v] is [{loc = sp; value = v}]. *)
val locate_opt : t option -> 'a -> 'a located

(** [locate sp v] is [{loc = Some sp; value = v}]. *)
val locate : t -> 'a -> 'a located

(** {1 Other Helper Functions} *)

(** [title src] gets the title of the source. The title of a [`File] source is its file path. *)
val title : source -> string option

(** {1 Support of Lexing} *)

(** [of_lex_position pos] converts an OCaml lexer position [pos] of type {!type:Lexing.position} into a {!type:position}. The input [pos] must be byte-indexed. (Therefore, the OCaml tool [ocamllex] is compatible, but the OCaml library [sedlex] is not because it uses Unicode code points.) *)
val of_lex_position : Lexing.position -> position

(** [of_lex_span (begining, ending)] takes a pair of OCaml lexer positions and creates a span. It is [make (of_lex_position begining, of_lex_position ending)]. *)
val of_lex_span : Lexing.position * Lexing.position -> t

(** [of_lexbuf lexbuf] constructs a span from the current lexeme that [lexbuf] points to. It is [of_lex_span (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)]. *)
val of_lexbuf : Lexing.lexbuf -> t

(** [locate_lex ps v] is a helper function to create a value annotated with a span. It is [locate (Some (of_lex_span ps)) v] and is designed to work with the OCaml parser generator Menhir. You can add the following code to your Menhir grammar to generate annotated data:

    {v
%inline
locate(X):
  | e = X
    { Asai.Span.locate_lex $loc e }
v}
*)
val locate_lex : Lexing.position * Lexing.position -> 'a -> 'a located

(** {1 Debugging} *)

(** Ugly printer for debugging *)
val dump_source : Format.formatter -> source -> unit
