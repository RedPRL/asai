module type Handler = DiagnosticEmitter.Handler

module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  module type Handler = Handler with module Code := Code and module Diagnostic := Diagnostic

  (** [messagef ~loc ~additional_marks ~code format ...] constructs a diagnostic along with the backtrace frames recorded via [tracef]. *)
  val messagef : ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, Diagnostic.t) format4 -> 'a

  (** [kmessagef kont ~loc ~additional_marks ~code format ...] constructs a diagnostic and then apply [kont] to the resulting diagnostic. *)
  val kmessagef : (Diagnostic.t -> 'b) -> ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** [tracef ~loc format ...] record a frame. *)
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a

  (** [append_marks msg marks] appends [marks] to the additional marks of [msg]. *)
  val append_marks : Diagnostic.t -> Span.t list -> Diagnostic.t

  (** [emitf ~loc ~additional_marks ~code format ...] constructs and emits a diagnostic. *)
  val emitf : ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, unit) format4 -> 'a

  (** [fatalf ~loc ~additional_marks ~code format ...] constructs a diagnostic and abort the current computation. *)
  val fatalf : ?loc:Span.t -> ?additional_marks:Span.t list -> ?severity:Severity.t -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

  module Run (H : Handler) :
  sig
    val run : (unit -> H.result) -> H.result
  end

  module TryWith (H : Handler) :
  sig
    val try_with : (unit -> H.result) -> H.result
  end

  module Perform :
  sig
    val emit : Diagnostic.t -> unit
    val fatal : Diagnostic.t -> 'a
  end
end
