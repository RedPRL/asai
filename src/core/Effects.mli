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

  (** Run the effects, and gather all diagostics encountered into a list. *)
  val run : (unit -> unit) -> Diagnostic.t list

  (** Run the effects, and display any errors we encounter. *)
  val run_display : display:Diagnostic.display -> (unit -> unit) -> int
end
