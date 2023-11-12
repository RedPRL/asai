include DebuggerSigs

module Make () = struct
  type 'a Effect.t +=
    | Debug : Loctext.t -> unit Effect.t
    | CallBegin : Loctext.t -> unit Effect.t
    | CallEnd : Loctext.t -> unit Effect.t

  let emit_loctext t = Effect.perform @@ Debug t
  let emit ?loc s = emit_loctext @@ Loctext.make ?loc s
  let emitf ?loc = Loctext.kmakef ?loc emit_loctext

  let trace_open_loctext t = Effect.perform @@ CallBegin t
  let trace_close_loctext t = Effect.perform @@ CallEnd t

  let trace ?loc s f =
    trace_open_loctext (Loctext.make ?loc s);
    Fun.protect f
      ~finally:(fun () -> trace_close_loctext (Loctext.make ?loc s))
  let tracef ?loc =
    Text.kmakef @@ fun t f ->
    trace_open_loctext (Range.locate_opt loc t);
    Fun.protect f ~finally:(fun () -> trace_close_loctext (Range.locate_opt loc t))
end
