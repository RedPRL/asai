(** Various location types.

    Before we get into the position types, it's a good idea to discuss
    some of the pitfalls regarding source positions and Unicode.
    Many concepts such as column numbers and span width
    become somewhat malformed when dealing with Unicode text.
    The problem is that these are fundamentally {i relative} concepts,
    insofar that they are dependent on various font-dependent factors.
    For instance, the existence of ZWJ sequences makes it impossible
    to determine how wide a user will percieve a sequence of characters.

    Therefore, Kai only exposes semantically meaningful operations. When
    performing any sort of user-facing display, the only truly meaningful
    thing we can do is get a slice of what the user typed.
*)

(** {1 Positions} *)
module Pos : sig
  (** The abstract type of positions. *)
  type t

  (** Get the filename associated with the position. *)
  val filename : t -> string

  (** Get the 1-indexed line number of the position. *)
  val line : t -> int

  (** Extract the line containing the position our of a UTF-8 encoded string. *)
  val utf8_slice_line : string -> t -> string
end

(** {1 Source Spans} *)
module Span : sig
  (** The abstract type of spans. *)
  type t

  (** Create a source span from a pair of lexer positions.
      Note that we expect these to be {i byte-indexed}, and they
      must come from the same file. *)
  val create : Lexing.position -> Lexing.position -> t

  (** {2 Accessors} *)

  (** Get the filename associated with the span. *)
  val filename : t -> string

  (** Get the 1-indexed line number of the start of the span. *)
  val start_line : t -> int

  (** Get the 1-indexed line number of the end of the span. *)
  val stop_line : t -> int

  (** {2 Slicing} *)

  (** Extract the span out of a UTF-8 encoded string.  *)
  val utf8_slice : string -> t -> string

  (** Extract the lines containing the span out of a UTF-8 encoded string. *)
  val utf8_slice_lines : string -> t -> string

  (** {3 Conversions} *)

  (** Get the start position of a span. *)
  val start_pos : t -> Pos.t

  (** Get the end position of a span. *)
  val stop_pos : t -> Pos.t
end

(** {1 Located Items} *)
module Loc : sig
  type 'a t = { span : Span.t; value : 'a }

  val value : 'a t -> 'a
end
