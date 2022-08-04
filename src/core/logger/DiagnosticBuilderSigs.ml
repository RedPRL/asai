module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  val messagef : ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, Diagnostic.t) format4 -> 'a
  val kmessagef : (Diagnostic.t -> 'b) -> ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a
  val append_marks : Diagnostic.t -> Span.t list -> Diagnostic.t

  val run : (unit -> 'a) -> 'a
end
