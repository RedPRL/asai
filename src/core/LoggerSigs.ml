module type S =
sig
  module Code : Diagnostic.Code

  (** {2 Constructing Diagnostics} *)

  (** [diagnosticf code format ...] constructs a diagnostic along with the backtrace frames recorded via [tracef].

      Example:
      {[
        Logger.diagnosticf `TypeError "Term %a does not type check" Syntax.pp tm
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param additional_marks Additional text fragments to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
  *)
  val diagnosticf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, Code.t Diagnostic.t) format4 -> 'a

  (** [kdiagnosticf kont code format ...] is [kont (diagnosticf code format ...)].

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param additional_marks Additional text fragments to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
  *)
  val kdiagnosticf : (Code.t Diagnostic.t -> 'b) -> ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** {2 Sending Diagnostics} *)

  (** Emit a diagnostic and continue the computation. *)
  val emit : Code.t Diagnostic.t -> unit

  (** [emitf code format ...] constructs and emits a diagnostic.

      Example:
      {[
        Logger.emitf `TypeError "Term %a does not type check" Syntax.pp tm
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param additional_marks Additional text fragments to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
  *)
  val emitf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, unit) format4 -> 'a

  (** Abort the computation with a diagnostic. *)
  val fatal: Code.t Diagnostic.t -> 'a

  (** [fatalf code format ...] constructs a diagnostic and aborts the current computation with the diagnostic.

      Example:
      {[
        Logger.fatalf `FileError "Failed to read %s" filepath
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param additional_marks Additional text fragments to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
  *)
  val fatalf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** {2 Backtraces} *)

  (** [get_backtrace()] returns the current backtrace. *)
  val get_backtrace : unit -> Diagnostic.backtrace

  (** [retrace bt f] runs the thunk [f] with the backtrace [bt].

      Example:
      {[
        (* running code with a fresh backtrace *)
        retrace Emp @@ fun () -> ...
      ]}
  *)
  val retrace : Diagnostic.backtrace -> (unit -> 'a) -> 'a

  (** [tracef frame f] records the [frame] and runs the thunk [f] with the new backtrace. It is [retrace (get_backtrace() <: fr) f] *)
  val trace : Diagnostic.message -> (unit -> 'a) -> 'a

  (** [tracef format ... f] constructs and records a frame, and runs the thunk [f] with the new backtrace.

      @param loc The location of the text (usually the code) to highlight.
  *)
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a

  (** {2 Algebraic Effects} *)

  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle non-fatal diagnostics before continuing the computation (see {!val:emit} and {!val:emitf}), and [fatal] to handle fatal diagnostics that have aborted the computation (see {!val:fatal} and {!val:fatalf}).

      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.
      @param emit The handler of non-fatal diagnostics.
      @param fatal The handler of fatal diagnostics. *)
  val run : ?init_backtrace:Diagnostic.backtrace -> emit:(Code.t Diagnostic.t -> unit) -> fatal:(Code.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  (** [adopt m run f] runs the thunk [f] that uses a different [Logger] instance, with the help of the runner [run] from that [Logger] instance, and then uses [m] to map the diagnostics generated by [f] into the ones in the current [Logger] instance. The backtrace within [f] will include the backtrace that leads to [adopt]. The intended use case is to integrate diagnostics from a library into those in the main application.

      [adopt] is a convenience function that can be implemented as follows:
      {[
        let adopt m f run =
          run ?init_backtrace:(Some (get_backtrace()))
            ~emit:(fun d -> emit (m d))
            ~fatal:(fun d -> fatal (m d))
            f
      ]}

      Here shows the intended usage, where [Lib] is the library to be used in the main application:
      {[
        module MainLogger = Logger.Make(Code)
        module LibLogger = Lib.Logger

        let _ = MainLogger.adopt (Diagnostic.map code_mapper) LibLogger.run @@ fun () -> ...
      ]}

      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.

  *)
  val adopt : ('code Diagnostic.t -> Code.t Diagnostic.t) -> (?init_backtrace:Diagnostic.backtrace -> emit:('code Diagnostic.t -> unit) -> fatal:('code Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a

  (** [try_with ~emit ~fatal f] runs the thunk [f], using [emit] to intercept non-fatal diagnostics before continuing the computation (see {!val:emit} and {!val:emitf}), and [fatal] to intercept fatal diagnostics that have aborted the computation (see {!val:fatal} and {!val:fatalf}). The default interceptors re-emit or re-raise the intercepted diagnostics.

      @param emit The interceptor of non-fatal diagnostics. The default value is {!val:emit}.
      @param fatal The interceptor of fatal diagnostics. The default value is {!val:fatal}. *)
  val try_with : ?emit:(Code.t Diagnostic.t -> unit) -> ?fatal:(Code.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
end
