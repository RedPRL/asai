module type Handler =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  type result
  val print : Diagnostic.t -> unit
  val fatal : Diagnostic.t -> result
end

module type S =
sig
  module Code : Code.S
  module Diagnostic : Diagnostic.S with module Code := Code

  module type Handler = Handler with module Code := Code and module Diagnostic := Diagnostic

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

  module Perform =
  struct
    type _ Effect.t += Print : D.t -> unit Effect.t
    exception Fatal of D.t
    let print d = Effect.perform @@ Print d
    let fatal d = raise @@ Fatal d
  end
  open Perform

  module Run (H : Handler) =
  struct
    let handler : (H.result, H.result) Effect.Deep.handler =
      { retc = Fun.id;
        exnc = (function Fatal d -> fatal d | exn -> raise exn);
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Print d -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
            Algaeff.Fun.Deep.finally k @@ fun () -> H.print d
          | _ -> None }

    let run f = Effect.Deep.match_with f () handler
  end

  module TryWith (H : Handler) =
  struct
    module R = Run (H)
    let try_with f = Effect.Deep.match_with f () R.handler
  end
end
