(** The minimum interface to use diagnostic handlers.

    Any module implementing {{!module-type:Asai.Reporter.Message}[Reporter.Message]} or {!module-type:StructuredReporter.Message} will also implement this minimum interface. *)
module type Message =
sig

  (** The type of all messages from the library. *)
  type t

  (** A concise, ideally Google-able string representation of each message from the library. *)
  val short_code : t -> string
end

(** The minimum interface for libraries and applications to work together.

    {b If you are the author of a library that uses asai,} it is recommended to wrap your reporter with this signature in the wrapper. For example, if you are using Dune, and your wrapper file is [CoolLibrary.ml(i)], use this to wrap your [Reporter]:
    {[
      module Reporter : Asai.MinimumSigs.Reporter
    ]}

    {b If you arrive here because the library you are using employs this template signature} to reveal only the minimum API, you can write the following code to connect the library's reporter to yours:
    {[
      Reporter.adopt (Diagnostic.map message_mapper) CoolLibrary.Reporter.run @@ fun () -> ...
    ]}
    Please take a look at our {{!page:quickstart}Quick Start Tutorial} about how to use a library that uses asai.
*)
module type Reporter =
sig

  module Message : Message
  (** The module for messages. *)

  val run : ?init_loc:Range.t -> ?init_backtrace:Diagnostic.backtrace -> emit:(Message.t Diagnostic.t -> unit) -> fatal:(Message.t Diagnostic.t -> 'a) -> (unit -> 'a) -> 'a
  (** [run ~emit ~fatal f] runs the thunk [f], using [emit] to handle non-fatal diagnostics before continuing the computation, and [fatal] to handle fatal diagnostics that have aborted the computation. The recommended way to handle messages from a library that uses asai is to use {{!val:Asai.Reporter.S.adopt}[adopt]}:
      {[
        Reporter.adopt (Diagnostic.map message_mapper) The_library.Reporter.run @@ fun () -> ...
      ]}

      @param init_loc The initial default location for messages.
      @param init_backtrace The initial backtrace to start with. The default value is the empty backtrace.
      @param emit The handler of non-fatal diagnostics.
      @param fatal The handler of fatal diagnostics. *)
end
