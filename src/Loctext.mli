include Explanation

(** {1 Functors} *)

module Make (Location : Location.S) : S with Location := Location and type t = Text.t Location.located

(** {1 Types} *)

(** A located text *)
type t = Text.t Range.located

(** {1 Builders} *)

(** [make str] converts the string [str] into a located text.

    @param loc The location of the text (usually the code) to highlight. *)
val make : ?loc:Range.t -> string -> t

(** [makef format ...] constructs a located text. Note that there should not be any literal control characters (e.g., literal newline characters).

    @param loc The location of the text (usually the code) to highlight.
*)
val makef : ?loc:Range.t -> ('a, Format.formatter, unit, t) format4 -> 'a

(** [kmakef kont format ...] is [kont (makef format ...)].

    @param loc The location of the text (usually the code) to highlight.
*)
val kmakef : ?loc:Range.t -> (t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
