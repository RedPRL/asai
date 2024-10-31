
module type S =
sig
  module Location : Location.S

  type t

  val make : ?loc:Location.t -> string -> t
  val makef : ?loc:Location.t -> ('a, Format.formatter, unit, t) format4 -> 'a
  val kmakef : ?loc:Location.t -> (t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
end
