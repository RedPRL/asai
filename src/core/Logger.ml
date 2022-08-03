module type Handler = DiagnosticEmitter.Handler

module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  module type Handler = Handler with module Code := Code and module Diagnostic := Diagnostic

  val messagef : ?loc:Span.t -> ?additional_marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, Diagnostic.t) format4 -> 'a
  val kmessagef : (Diagnostic.t -> 'b) -> ?loc:Span.t -> ?additional_marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  val tracef : ?loc:Span.t -> ('a, Format.formatter, unit, (unit -> 'b) -> 'b) format4 -> 'a
  val append_marks : Diagnostic.t -> Span.t list -> Diagnostic.t
  val printf : ?loc:Span.t -> ?additional_marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val fatalf : ?loc:Span.t -> ?additional_marks:Span.t list -> code:Code.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

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
    val print : Diagnostic.t -> unit
    val fatal : Diagnostic.t -> 'a
  end
end

module Make (C : Code.S) (D : Diagnostic.S with module Code := C) :
  S with module Code := C and module Diagnostic := D =
struct
  module type Handler = Handler with module Code := C and module Diagnostic := D 

  module DE = DiagnosticEmitter.Make(C)(D)
  module DB = DiagnosticBuilder.Make(C)(D)

  let messagef = DB.messagef
  let kmessagef = DB.kmessagef
  let tracef = DB.tracef
  let append_marks = DB.append_marks
  let printf ?loc ?additional_marks ~code = DB.kmessagef DE.Perform.print ?loc ?additional_marks ~code
  let fatalf ?loc ?additional_marks ~code = DB.kmessagef DE.Perform.fatal ?loc ?additional_marks ~code

  module Run (H : Handler) =
  struct
    module DERun = DE.Run (H)
    let run f = DB.run @@ fun () -> DERun.run f 
  end

  module TryWith = DE.TryWith
  module Perform = DE.Perform
end
