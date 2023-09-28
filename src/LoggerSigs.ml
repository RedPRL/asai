module type S =
sig
  module Code : Diagnostic.Code

  (** {2 Sending Messages} *)

  (** [emit code str] emits a string and continues the computation.

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

  (** [emitf code format ...] formats and emits a message, and then continues the computation. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

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

  (** [with_backtrace bt f] runs the thunk [f] with [bt] as the initial backtrace.

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

  (** [trace_text text f] records the message [text] and runs the thunk [f] with the new backtrace.

      @param loc The location of the text (usually the code) to highlight. *)
  val trace_text : ?loc:Span.t -> Diagnostic.text -> (unit -> 'a) -> 'a

  (** [trace_message msg f] records the message [msg] and runs the thunk [f] with the new backtrace. *)
  val trace_message : Diagnostic.message -> (unit -> 'a) -> 'a

  (** {2 Locations} *)

  (** [get_loc()] returns the current location. *)
  val get_loc : unit -> Span.t option

  (** [with_loc loc f] runs the thunk [f] with [loc] as the initial location [loc]. Note that [with_loc None] will clear the current location, while [merge_loc None] will keep it. See {!val:merge_loc}. *)
  val with_loc : Span.t option -> (unit -> 'a) -> 'a

  (** [merge_loc loc f] "merges" [loc] into the current location and runs the thunk [f]. By "merge", it means that if [loc] is [None], then the current location is kept; otherwise, it is overwritten. Note that [with_loc None] will clear the current location, while [merge_loc None] will keep it. See {!val:with_loc}. *)
  val merge_loc : Span.t option -> (unit -> 'a) -> 'a

  (** {2 Constructing Diagnostics} *)

  (** Functions in this section differ from the ones in {!module:Diagnostic} (for example, {!val:Diagnostic.make}) in that they fill out the current location, the current backtrace, and the severity automatically. (One can still overwrite them with optional arguments.) *)

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
  val run : ?init_loc:Span.t -> ?init_backtrace:Diagnostic.backtrace -> emit:(Code.t Diagnostic.t -> unit) -> fatal:(Code.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  (** [adopt m run f] runs the thunk [f] that uses a {i different} [Logger] instance. It takes the runner [run] from that [Logger] instance as an argument to handle effects, and will use [m] to transform diagnostics generated by [f] into ones in the current [Logger] instance. The backtrace within [f] will include the backtrace that leads to [adopt], and the innermost specified location will be carried over, too. The intended use case is to integrate diagnostics from a library into those in the main application.

      [adopt] is a convenience function that can be implemented as follows:
      {[
        let adopt m f run =
          run
            ?init_loc:(get_loc())
            ?init_backtrace:(Some (get_backtrace()))
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
  *)
  val adopt : ('code Diagnostic.t -> Code.t Diagnostic.t) -> (?init_loc:Span.t -> ?init_backtrace:Diagnostic.backtrace -> emit:('code Diagnostic.t -> unit) -> fatal:('code Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a

  (** [try_with ~emit ~fatal f] runs the thunk [f], using [emit] to intercept non-fatal diagnostics before continuing the computation (see {!val:emit} and {!val:emitf}), and [fatal] to intercept fatal diagnostics that have aborted the computation (see {!val:fatal} and {!val:fatalf}). The default interceptors re-emit or re-raise the intercepted diagnostics.

      @param emit The interceptor of non-fatal diagnostics. The default value is {!val:emit_diagnostic}.
      @param fatal The interceptor of fatal diagnostics. The default value is {!val:fatal_diagnostic}. *)
  val try_with : ?emit:(Code.t Diagnostic.t -> unit) -> ?fatal:(Code.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  val register_printer : ([ `Trace | `Emit of Code.t Diagnostic.t | `Fatal of Code.t Diagnostic.t ] -> string option) -> unit
  (** [register_printer p] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects and exceptions into strings for the OCaml runtime system to display. Ideally, all internal effects and exceptions should have been handled by {!val:run} and there is no need to use this function, but when it is not the case, this function can be helpful for debugging. The functor {!module:Logger.Make} always registers a simple printer to suggest using {!val:run}, but you can register new ones to override it. The return type of the printer [p] should return [Some s] where [s] is the resulting string, or [None] if it chooses not to convert a particular effect or exception. The registered printers are tried in reverse order until one of them returns [Some s] for some [s]; that is, the last registered printer is tried first. Note that this function is a wrapper of {!val:Printexc.register_printer} and all the registered printers (via this function or {!val:Printexc.register_printer}) are put into the same list.

      The input type of the printer [p] is a variant representation of all internal effects and exceptions used in this module:
      - [`Trace] corresponds to the effect triggered by {!val:trace}; and
      - [`Emit diag] corresponds to the effect triggered by {!val:emit}; and
      - [`Fatal diag] corresponds to the exception triggered by {!val:fatal}.

      Note: {!val:Diagnostic.string_of_text} can be handy for converting a message into a string.
  *)
end
