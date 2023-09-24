module type S =
sig
  module Code : Diagnostic.Code

  (** {2 Sending Diagnostics} *)

  (** [emit code str] emits a string and continue the computation.

      Example:
      {[
        Logger.emit `TypeError "This type is extremely unnatural:\nNat"
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val emit : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> string -> unit

  (** [emitf code format ...] formats and emits a message, and then continue the computation. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      Example:
      {[
        Logger.emitf `TypeError "Type %a is too ugly" Syntax.pp tp
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val emitf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, unit) format4 -> 'a

  (** Emit a diagnostic and continue the computation. *)
  val emit_diagnostic : Code.t Diagnostic.t -> unit

  (** [fatal code str] aborts the current computation with the string [str].

      Example:
      {[
        Logger.fatal `FileError "Forgot to feed the cat"
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val fatal : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> string -> 'a

  (** [fatalf code format ...] constructs a diagnostic and aborts the current computation with the diagnostic. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      Example:
      {[
        Logger.fatalf `FileError "Failed to write the password to %s" file_path
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val fatalf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** Abort the computation with a diagnostic. *)
  val fatal_diagnostic: Code.t Diagnostic.t -> 'a

  (** {2 Backtraces} *)

  (** [get_backtrace()] returns the current backtrace. *)
  val get_backtrace : unit -> Diagnostic.backtrace

  (** [with_backtrace bt f] runs the thunk [f] with the backtrace [bt].

      Example:
      {[
        (* running code with a fresh backtrace *)
        with_backtrace Emp @@ fun () -> ...
      ]}
  *)
  val with_backtrace : Diagnostic.backtrace -> (unit -> 'a) -> 'a

  (** [trace str f] records the string [str] and runs the thunk [f] with the new backtrace.

      @param loc The location of the text (usually the code) to highlight.
  *)
  val trace : ?loc:Span.t -> string -> (unit -> 'a) -> 'a

  (** [tracef format ... f] formats and records a message as a frame in the backtrace, and runs the thunk [f] with the new backtrace. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      @param loc The location of the text (usually the code) to highlight.
  *)
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a

  (** [trace_message msg f] records the message [msg] and runs the thunk [f] with the new backtrace. *)
  val trace_message : Diagnostic.message -> (unit -> 'a) -> 'a

  (** {2 Constructing Diagnostics with Current Backtraces} *)

  (** [diagnostic code str] constructs a diagnostic with the message [str] along with the backtrace frames recorded via {!val:tracef}.

      Example:
      {[
        Logger.diagnostic `TypeError "This\nis\ntoo\nmuch."
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val diagnostic : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> string -> Code.t Diagnostic.t

  (** [diagnosticf code format ...] constructs a diagnostic along with the backtrace frames recorded via {!val:trace}. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      Example:
      {[
        Logger.diagnosticf `TypeError "Term %a does not type check, or does it?" Syntax.pp tm
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val diagnosticf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, Code.t Diagnostic.t) format4 -> 'a

  (** [kdiagnosticf kont code format ...] is [kont (diagnosticf code format ...)]. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val kdiagnosticf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> (Code.t Diagnostic.t -> 'b) -> Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

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
            ~emit:(fun d -> emit_diagnostic (m d))
            ~fatal:(fun d -> fatal_diagnostic (m d))
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

      @param emit The interceptor of non-fatal diagnostics. The default value is {!val:emit_diagnostic}.
      @param fatal The interceptor of fatal diagnostics. The default value is {!val:fatal_diagnostic}. *)
  val try_with : ?emit:(Code.t Diagnostic.t -> unit) -> ?fatal:(Code.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  val register_printer : ([ `Emit of Code.t Diagnostic.t | `Fatal of Code.t Diagnostic.t ] -> string option) -> unit
  (** [register_printer p] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects and exceptions into strings for the OCaml runtime system to display. Ideally, all internal effects and exceptions should have been handled by {!val:run} and there is no need to use this function, but when it is not the case, this function can be helpful for debugging. The functor {!module:Logger.Make} always registers a simple printer to suggest using {!val:run}, but you can register new ones to override it. The return type of the printer [p] should return [Some s] where [s] is the resulting string, or [None] if it chooses not to convert a particular effect or exception. The registered printers are tried in reverse order until one of them returns [Some s] for some [s]; that is, the last registered printer is tried first. Note that this function is a wrapper of {!val:Printexc.register_printer} and all the registered printers (via this function or {!val:Printexc.register_printer}) are put into the same list.

      The input type of the printer [p] is a variant representation of all internal effects and exceptions used in this module:
      - [`Emit diag] corresponds to the effect triggered by {!val:emit}; and
      - [`Fatal diag] corresponds to the exception triggered by {!val:fatal}.

      Note: {!val:Diagnostic.string_of_text} can be handy for converting a message into a string.
  *)
end
