module type S =
sig
  val debug : ?loc:Range.t -> string -> unit
  val debugf : ?loc:Range.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val stalk : ?loc:Range.t -> string -> (unit -> 'a) -> 'a
  val stalkf : ?loc:Range.t -> ('b, Format.formatter, unit, (unit -> 'a) -> 'a) format4 -> 'b
end
