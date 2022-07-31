module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  val messagef : ?loc:Span.t -> ?marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, Diagnostic.t) format4 -> 'a
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a
  val append_marks : Diagnostic.t -> Span.t list -> Diagnostic.t

  val run : (unit -> 'a) -> 'a
end

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D
