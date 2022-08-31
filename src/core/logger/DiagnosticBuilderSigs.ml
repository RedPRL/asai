module type S =
sig
  module Code : Code.S
  module Phase : Phase.S

  val backtrace : unit -> (Phase.t * Diagnostic.message Span.located) Bwd.bwd
  val messagef : ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, (Code.t,Phase.t) Diagnostic.t) format4 -> 'a
  val kmessagef : ((Code.t,Phase.t) Diagnostic.t -> 'b) -> ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  val tracef : ?loc:Span.t -> Phase.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a
  val append_marks : (Code.t,Phase.t) Diagnostic.t -> Span.t list -> (Code.t,Phase.t) Diagnostic.t

  val run : ?init:((Phase.t * Diagnostic.message Span.located) Bwd.bwd) -> (unit -> 'a) -> 'a
end
