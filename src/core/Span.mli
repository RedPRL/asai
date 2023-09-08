(** The type of locations and single spans. *)

(** {1 Types} *)

(** The type of positions *)
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

(** An auxiliary type to package data with spans. This can be useful for specifying the location of a token. *)
type 'a located = { loc : t option; value : 'a }

(** {1 Builders} *)

(** [make beginning ending] builds the span [\[begining, ending)] (not including the byte at the ending position) from a pair of positions [beginning] and [ending].

    @raise Invalid_argument if the positions do not share the same file path or if [end_] comes before [begin_]. (It is okay if [end_] equals to [begin_], which means the span is empty.) The comparison of file paths is done by [String.equal] without any path normalization.
*)
val make : position -> position -> t

(** [of_lex_position pos] converts an OCaml lexer position [pos] of type {!type:Lexing.position} into a {!type:position}. The input [pos] must be byte-indexed. (Therefore, the OCaml tool [ocamllex] is compatible, but the OCaml library [sedlex] is not because it uses Unicode code points.) *)
val of_lex_position : Lexing.position -> position

(** [of_lex lexbuf] is [make (of_lex_position (Lexing.lexeme_start_p lexbuf)) (of_lex_position (Lexing.lexeme_end_p lexbuf))], a convenience function. *)
val of_lex : Lexing.lexbuf -> t

(** [to_start_of_line pos] returns the position at the start of the line. It is an idempotent function. *)
val to_start_of_line : position -> position

(** [to_positions] is the right inverse of {!val:make} (up to currying and uncurrying). [to_positions span] returning the pair of the beginning and ending positions of [span]. *)
val to_positions : t -> position * position

(** {1 Accessors} *)

(** [file_path span] returns the file path associated with [span]. *)
val file_path : t -> string

(** [begin_line_num span] returns the 1-indexed line number of the beginning position. *)
val begin_line_num : t -> int

(** [end_line_num span] returns the 1-indexed line number of the ending position. *)
val end_line_num : t -> int
