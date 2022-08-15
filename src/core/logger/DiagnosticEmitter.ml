module type S = DiagnosticEmitterSigs.S

module Make (Code : Code.S) : S with module Code := Code =
struct
  type _ Effect.t += Emit : Code.t Diagnostic.t -> unit Effect.t
  exception Fatal of Code.t Diagnostic.t

  let emit d = Effect.perform @@ Emit d
  let fatal d = raise @@ Fatal d

  let handler ~(emit : _ -> unit) ~fatal : _ Effect.Deep.handler =
    { retc = Fun.id;
      exnc = (function Fatal d -> fatal d | exn -> raise exn);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Emit d -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
          Algaeff.Fun.Deep.finally k @@ fun () -> emit d
        | _ -> None }

  let run ~emit ~fatal f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal

  let try_with ?(emit=emit) ?(fatal=fatal) f =
    Effect.Deep.match_with f () @@ handler ~emit ~fatal
end
