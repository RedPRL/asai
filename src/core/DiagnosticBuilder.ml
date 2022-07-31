open Bwd
open BwdNotation

module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  val messagef : ?loc:Span.t -> ?marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, Diagnostic.t) format4 -> 'a
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a
  val append_marks : Diagnostic.t -> Span.t list -> Diagnostic.t

  val run : (unit -> 'a) -> 'a
end

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D =
struct
  type env = Diagnostic.message Span.located bwd
  module Traces = Algaeff.Reader.Make (struct type nonrec env = env end)

  let messagef ?loc ?(marks=[]) ~code =
    Format.kdprintf @@ fun message ->
    D.{
      code;
      message = {loc; value = message};
      additional_marks = marks;
      traces = Traces.read ();
    }

  let append_marks d marks =
    D.{ d with additional_marks = d.additional_marks @ marks }

  let tracef ?loc fmt =
    fmt |> Format.kdprintf @@ fun message f ->
    Traces.scope (fun bt -> bt #< { loc; value = message }) f

  let run f = Traces.run ~env:Emp f

end
