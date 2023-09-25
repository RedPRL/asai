(** {1 Types} *)

(** The type of positions; this is isomorphic to {!type:Lexing.position}, but with arguably better field names. *)
type position = {
  file_path : string;
  (** The absolute file path of the file that contains the position. *)

  offset : int;
  (** The 0-indexed byte offset of the position relative to the beginning of the file. *)

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

(** [make (beginning, ending)] builds the span [\[begining, ending)] (not including the byte at the ending position) from a pair of positions [beginning] and [ending].

    @raise Invalid_argument if the positions do not share the same file path or if [end_] comes before [begin_]. (It is okay if [end_] equals to [begin_], which means the span is empty.) The comparison of file paths is done by [String.equal] without any path normalization.
*)
val make : position * position -> t

(** [split span] returning the pair of the beginning and ending positions of [span]. It is the right inverse of {!val:make}. *)
val split : t -> position * position

(** [file_path span] returns the file path associated with [span]. *)
val file_path : t -> string

(** [begin_line_num span] returns the 1-indexed line number of the beginning position. *)
val begin_line_num : t -> int

(** [end_line_num span] returns the 1-indexed line number of the ending position. *)
val end_line_num : t -> int

(** [begin_offset span] returns the 0-indexed offset of the (inclusive) beginning position. *)
val begin_offset : t -> int

(** [end_offset span] returns the 0-indexed offset of the (exclusive) ending position. *)
val end_offset : t -> int

(** [locate sp v] is [{loc = sp; value = v}]. *)
val locate : t option -> 'a -> 'a located

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
