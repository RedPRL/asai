(** The signature of structured messages. An implementer should specify the structured messages used in their library or application. *)
module type Message =
sig
  (** The type of all structured messages. *)
  type t

  (** The default severity level of a message. Severity levels classify diagnostics into errors, warnings, etc. It is about how serious the {i end user} should take the diagnostic, not whether the program should stop or continue. The severity may be overwritten at the time of issuing a diagnostic. *)
  val default_severity : t -> Diagnostic.severity

  (** The default text of the message. This is the long explanation of the message that the end user would see. You might find helper functions {!val:Diagnostic.text} and {!val:Diagnostic.textf} useful. The text may be overwritten at the time of issuing a diagnostic. *)
  val default_text : t -> Diagnostic.text

  (** A concise, ideally Google-able string representation of each message. Detailed or long descriptions should be avoided---the shorter, the better. For example, [E001] works better than [type-checking error]. It will be assumed that the string representation has no control characters (such as newline characters). *)
  val short_code : t -> string
end

module type S =
sig
  module Message : Message

  (** {2 Sending Messages} *)

  (** [emit message] emits the [message] and continues the computation.

      Example:
      {[
        Reporter.emit @@ TypeError (tm, ty)
      ]}

      @param severity The severity (to overwrite the default severity inferred from the [message]).
      @param loc The location of the text (usually the code) to highlight. The default value is the innermost location given by {!val:trace}, {!val:with_loc}, {!val:merge_loc}, or {!run}.
      @param text The text (to overwrite the default text inferred from the message [msg]).
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param extra_remarks Additional remarks that are not part of the backtrace.
  *)
  val emit : ?severity:Diagnostic.severity -> ?loc:Range.t -> ?text:Diagnostic.text -> ?backtrace:Diagnostic.backtrace -> ?extra_remarks:Diagnostic.loctext list -> Message.t -> unit

  (** Emit a diagnostic and continue the computation. *)
  val emit_diagnostic : Message.t Diagnostic.t -> unit

  (** [fatal message] aborts the current computation with the [message].

      Example:
      {[
        Reporter.fatal @@ CatError "forgot to feed the cat"
      ]}

      @param severity The severity (to overwrite the default severity inferred from the [message]).
      @param loc The location of the text (usually the code) to highlight. The default value is the innermost location given by {!val:trace}, {!val:with_loc}, {!val:merge_loc}, or {!run}.
      @param text The text (to overwrite the default text inferred from the [message]).
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param extra_remarks Additional remarks that are not part of the backtrace.
  *)
  val fatal : ?severity:Diagnostic.severity -> ?loc:Range.t -> ?text:Diagnostic.text -> ?backtrace:Diagnostic.backtrace -> ?extra_remarks:Diagnostic.loctext list -> Message.t -> 'a

  (** Abort the computation with a diagnostic. *)
  val fatal_diagnostic: Message.t Diagnostic.t -> 'a

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

      @param loc The location of the text (usually the code) to highlight. The default value is [None]. Note that a non-[None] location given here will become the new default location for inner {!val:emit} and {!val:fatal}.
  *)
  val trace : ?loc:Range.t -> string -> (unit -> 'a) -> 'a

  (** [tracef format ... f] formats and records a frame in the backtrace, and runs the thunk [f] with the new backtrace. Note that there should not be any literal control characters. See {!type:Diagnostic.text}.

      @param loc The location of the text (usually the code) to highlight. The default value is [None]. Note that a non-[None] location given here will become the new default location for inner {!val:emit} and {!val:fatal}.
  *)
  val tracef : ?loc:Range.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a

  (** [trace_text text f] records the [text] and runs the thunk [f] with the new backtrace.

      @param loc The location of the text (usually the code) to highlight. The default value is [None]. Note that a non-[None] location given here will become the new default location for inner {!val:emit} and {!val:fatal}.
  *)
  val trace_text : ?loc:Range.t -> Diagnostic.text -> (unit -> 'a) -> 'a

  (** [trace_loctext loctext f] records the [loctext] and runs the thunk [f] with the new backtrace. Note that a non-[None] location given here will become the new default location for inner {!val:emit} and {!val:fatal}. *)
  val trace_loctext : Diagnostic.loctext -> (unit -> 'a) -> 'a

  (** {2 Locations} *)

  (** [get_loc()] returns the current default location for {!val:emit} and {!val:fatal}. *)
  val get_loc : unit -> Range.t option

  (** [with_loc loc f] runs the thunk [f] with [loc] as the new default location for {!val:emit} and {!val:fatal}. Note that [with_loc None] will clear the current default location, while [merge_loc None] will keep it. See {!val:merge_loc}. *)
  val with_loc : Range.t option -> (unit -> 'a) -> 'a

  (** [merge_loc loc f] "merges" [loc] into the current default location for {!val:emit} and {!val:fatal} and runs the thunk [f]. By "merge", it means that if [loc] is [None], then the current default location is kept; otherwise, it is overwritten. Note that [with_loc None] will clear the current default location, while [merge_loc None] will keep it. See {!val:with_loc}. *)
  val merge_loc : Range.t option -> (unit -> 'a) -> 'a

  (** {2 Constructing Diagnostics} *)

  (** Functions in this section differ from the ones in {!module:Diagnostic} (for example, {!val:Diagnostic.make}) in that they fill out the current location, the current backtrace, and the severity automatically. (One can still overwrite them with optional arguments.) *)

  (** [diagnostic message] constructs a diagnostic with the [message] along with the backtrace frames recorded via {!val:trace}.

      Example:
      {[
        Reporter.diagnostic @@ SyntaxError "too many emojis"
      ]}

      @param severity The severity (to overwrite the default severity inferred from the [message]).
      @param loc The location of the text (usually the code) to highlight. The default value is the innermost location given by {!val:trace}, {!val:with_loc}, {!val:merge_loc}, or {!run}.
      @param text The text (to overwrite the default text inferred from the [message]).
      @param backtrace The backtrace (to overwrite the accumulative frames up to this point).
      @param extra_remarks Additional remarks that are not part of the backtrace.
  *)
  val diagnostic : ?severity:Diagnostic.severity -> ?loc:Range.t -> ?text:Diagnostic.text -> ?backtrace:Diagnostic.backtrace -> ?extra_remarks:Diagnostic.loctext list -> Message.t -> Message.t Diagnostic.t

  (** {2 Algebraic Effects} *)

  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle non-fatal diagnostics before continuing the computation (see {!val:emit}), and [fatal] to handle fatal diagnostics that have aborted the computation (see {!val:fatal}).

      @param init_loc The initial default location for inner {!val:emit} and {!val:fatal}. The default value is [None].
      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.
      @param emit The handler of non-fatal diagnostics.
      @param fatal The handler of fatal diagnostics. *)
  val run : ?init_loc:Range.t -> ?init_backtrace:Diagnostic.backtrace -> emit:(Message.t Diagnostic.t -> unit) -> fatal:(Message.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  (** [adopt m run f] runs the thunk [f] that uses a {i different} [Reporter] instance. It takes the runner [run] from that [Reporter] instance as an argument to handle effects, and will use [m] to transform diagnostics generated by [f] into ones in the current [Reporter] instance. The backtrace within [f] will include the backtrace that leads to [adopt], and the innermost specified location will be carried over, too. The intended use case is to integrate diagnostics from a library into those in the main application.

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
        module LibReporter = Lib.Reporter

        let _ = Reporter.adopt (Diagnostic.map message_mapper) LibReporter.run @@ fun () -> ...
      ]}
  *)
  val adopt : ('message Diagnostic.t -> Message.t Diagnostic.t) -> (?init_loc:Range.t -> ?init_backtrace:Diagnostic.backtrace -> emit:('message Diagnostic.t -> unit) -> fatal:('message Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a

  (** [try_with ~emit ~fatal f] runs the thunk [f], using [emit] to intercept non-fatal diagnostics before continuing the computation (see {!val:emit}), and [fatal] to intercept fatal diagnostics that have aborted the computation (see {!val:fatal}). The default interceptors re-emit or re-raise the intercepted diagnostics.

      @param emit The interceptor of non-fatal diagnostics. The default value is {!val:emit_diagnostic}.
      @param fatal The interceptor of fatal diagnostics. The default value is {!val:fatal_diagnostic}. *)
  val try_with : ?emit:(Message.t Diagnostic.t -> unit) -> ?fatal:(Message.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a

  (** [map_diagnostic m f] runs the thunk [f] and applies [m] to any diagnostic sent by [f]. It is a convenience function that can be implemented as follows:
      {[
        let map_diagnostic m f =
          try_with
            ~fatal:(fun d -> fatal_diagnostic (m d))
            ~emit:(fun d -> emit_diagnostic (m d))
            f
      ]}

      @since 0.2.0
  *)
  val map_diagnostic : (Message.t Diagnostic.t -> Message.t Diagnostic.t) -> (unit -> 'a) -> 'a

  (** {2 Debugging} *)

  val register_printer : ([ `Trace | `Emit of Message.t Diagnostic.t | `Fatal of Message.t Diagnostic.t ] -> string option) -> unit
  (** [register_printer p] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects and exceptions into strings for the OCaml runtime system to display. Ideally, all internal effects and exceptions should have been handled by {!val:run} and there is no need to use this function, but when it is not the case, this function can be helpful for debugging. The functor {!module:Reporter.Make} always registers a simple printer to suggest using {!val:run}, but you can register new ones to override it. The return type of the printer [p] should return [Some s] where [s] is the resulting string, or [None] if it chooses not to convert a particular effect or exception. The registered printers are tried in reverse order until one of them returns [Some s] for some [s]; that is, the last registered printer is tried first. Note that this function is a wrapper of {!val:Printexc.register_printer} and all the registered printers (via this function or {!val:Printexc.register_printer}) are put into the same list.

      The input type of the printer [p] is a variant representation of all internal effects and exceptions used in this module:
      - [`Trace] corresponds to the effect triggered by {!val:trace}; and
      - [`Emit diag] corresponds to the effect triggered by {!val:emit}; and
      - [`Fatal diag] corresponds to the exception triggered by {!val:fatal}.

      Note: {!val:Diagnostic.string_of_text} can be handy for converting a text into a string.
  *)
end
