module type S =
sig
  module Code : Diagnostic.Code

  (** {2 Constructing Diagnostics} *)

  (** [diagnosticf code format ...] constructs a diagnostic along with the backtrace frames recorded via [tracef].

      Example:
      {[
        Logger.diagnosticf `TypeError "Term %a does not type check" Syntax.pp tm
      ]}

      Note that the format strings should not contain any control characters. See {!type:Diagnostic.text}.

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val diagnosticf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, Code.t Diagnostic.t) format4 -> 'a

  (** [kdiagnosticf kont code format ...] is [kont (diagnosticf code format ...)].

      Note that the format strings should not contain any control characters. See {!type:Diagnostic.text}.

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
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
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
  *)
  val emitf : ?severity:Diagnostic.severity -> ?loc:Span.t -> ?backtrace:Diagnostic.backtrace -> ?additional_messages:Diagnostic.message list -> Code.t -> ('a, Format.formatter, unit, unit) format4 -> 'a

  (** Abort the computation with a diagnostic. *)
  val fatal: Code.t Diagnostic.t -> 'a

  (** [fatalf code format ...] constructs a diagnostic and aborts the current computation with the diagnostic.

      Note that the format strings should not contain any control characters. See {!type:Diagnostic.text}.

      Example:
      {[
        Logger.fatalf `FileError "Failed to read %s" filepath
      ]}

      @param severity The severity (to overwrite the default severity inferred from the message [code]).
      @param loc The location of the text (usually the code) to highlight.
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param additional_messages Additional messages that part of the backtrace. For example, they can be bindings shadowed by the current one.
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

      Note that the format strings should not contain any control characters. See {!type:Diagnostic.text}.

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

  val register_printer : ([ `Emit of Code.t Diagnostic.t | `Fatal of Code.t Diagnostic.t ] -> string option) -> unit
  (** [register_printer f] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects and exceptions into strings for the OCaml runtime system to display them. The functor {!module:Logger.Make} always registers a simple printer to suggest using {!val:run}, but you can register new ones to override it. The return type of the printer [p] should return [Some s] where [s] is the resulting string, or [None] if it chooses not to convert a particular effect or exception. The registered printers are tried in reverse order until one of them returns [Some s] for some [s]; that is, the last registered printer is tried first. Note that this function is a wrapper of {!val:Printexc.register_printer} and all the registered printers (via this function or {!val:Printexc.register_printer}) are put into the same list.

      The input type of the printer [p] is a variant representation of all internal effects and exceptions used in this module:
      - [`Emit diag] corresponds to the effect triggered by {!val:emit}; and
      - [`Fatal diag] corresponds to the exception triggered by {!val:fatal}.

      Note: {!val:Diagnostic.string_of_text} can be handy for converting a message into a string.
  *)
end
