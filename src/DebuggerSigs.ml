module type S =
sig
  val emit : ?loc:Range.t -> string -> unit
  val emitf : ?loc:Range.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val trace : ?loc:Range.t -> string -> (unit -> 'a) -> 'a
  val tracef : ?loc:Range.t -> ('b, Format.formatter, unit, (unit -> 'a) -> 'a) format4 -> 'b
  val run : emit:(Loctext.t -> unit) -> trace:([`Open | `Close] -> Loctext.t -> unit) -> (unit -> 'a) -> 'a
end
