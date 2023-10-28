(** This signature presents the minimum interface to use diagnostic handlers. Any module implementing {!module-type:Reporter.Message} or {!module-type:StructuredReporter.Message} will also implement this minimum interface. *)
module type Message =
sig

  (** The type of all messages from the library. *)
  type t

  (** A concise, ideally Google-able string representation of each message from the library. *)
  val short_code : t -> string
end

(** This is a template signature provided by asai. It is the minimum interface for different libraries and applications that use asai to work together. The author of the library you are using employs this template to hide almost everything from the public interface except the minimum API for you to adopt it. In the following documentation, "the library" refers to the library using this template, not asai. *)
module type Reporter =
sig

  module Message : Message

  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle non-fatal diagnostics from the library before continuing the computation, and [fatal] to handle fatal diagnostics from the library that have aborted the computation. The recommended way to handle messages from the library is to use {!val:Asai.Reporter.S.adopt}:
      {[
        Reporter.adopt (Diagnostic.map message_mapper) The_library.Reporter.run @@ fun () -> ...
      ]}

      @param init_loc The initial default location for inner {!val:emit} and {!val:fatal}. The default value is [None].
      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.
      @param emit The handler of non-fatal diagnostics.
      @param fatal The handler of fatal diagnostics. *)
  val run : ?init_loc:Range.t -> ?init_backtrace:Diagnostic.backtrace -> emit:(Message.t Diagnostic.t -> unit) -> fatal:(Message.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
end
