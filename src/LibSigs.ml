module type Reporter =
sig
  module Message :
  sig
    (** The type of all messages from the library you use. *)
    type t

    (** The default severity level of a message from the library you use. *)
    val default_severity : t -> Diagnostic.severity

    (** A concise, ideally Google-able string representation of each message from this library. *)
    val short_code : t -> string
  end

  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle non-fatal diagnostics before continuing the computation (see {!val:emit} and {!val:emitf}), and [fatal] to handle fatal diagnostics that have aborted the computation (see {!val:fatal} and {!val:fatalf}).

      The recommended way to embed messages from a library using asai is to use {!val:Reporter.S.adopt}:
      {[
        let _ = Reporter.adopt (Diagnostic.map message_mapper) ThisLib.Reporter.run @@ fun () -> ...
      ]}

      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.
      @param emit The handler of non-fatal diagnostics.
      @param fatal The handler of fatal diagnostics. *)
  val run : ?init_loc:Range.t -> ?init_backtrace:Diagnostic.backtrace -> emit:(Message.t Diagnostic.t -> unit) -> fatal:(Message.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
end
