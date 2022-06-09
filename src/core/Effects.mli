open Loc


module Make (ErrorCode: ErrorCode.S) :
sig
  module Diagnostic : module type of Diagnostic.Make(ErrorCode)


  val build : code:ErrorCode.t -> string -> Diagnostic.t
  val cause : string -> Diagnostic.t -> Diagnostic.t

  val emit : Diagnostic.t -> unit
  val fatal : Diagnostic.t -> 'a

  val locate : Span.t -> (unit -> 'a) -> 'a
  val position : Pos.t -> (unit -> 'a) -> 'a

  val load_file : filepath:string -> string -> unit

  val run : display:Diagnostic.display -> (unit -> unit) -> int
end
