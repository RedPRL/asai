module type S =
sig
  (** Location *)
  module Location : Location.S

  (** The type of explanations *)
  type t

  (** Making an explanation from a string *)
  val make : ?loc:Location.t -> string -> t

  (** Making an explanation by formatting a string *)
  val makef : ?loc:Location.t -> ('a, Format.formatter, unit, t) format4 -> 'a

  (** Making an explanation by formatting a string, and taking a continuation *)
  val kmakef : ?loc:Location.t -> (t -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
end
