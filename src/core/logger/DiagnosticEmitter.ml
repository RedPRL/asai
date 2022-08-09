module type Handler = DiagnosticEmitterSigs.Handler
module type S = DiagnosticEmitterSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  module type Handler = Handler with module Code := Code

  module Perform =
  struct
    type _ Effect.t += Print : Code.t Diagnostic.t -> unit Effect.t
    exception Fatal of Code.t Diagnostic.t
    let emit d = Effect.perform @@ Print d
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
            Algaeff.Fun.Deep.finally k @@ fun () -> H.emit d
          | _ -> None }

    let run f = Effect.Deep.match_with f () handler
  end

  module TryWith (H : Handler) =
  struct
    module R = Run (H)
    let try_with f = Effect.Deep.match_with f () R.handler
  end
end
