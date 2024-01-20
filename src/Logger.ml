include LoggerSigs

type 'a Effect.t +=
  | Debug : Diagnostic.loctext -> unit Effect.t
  | CallBegin : Diagnostic.loctext -> unit Effect.t
  | CallEnd : Diagnostic.loctext -> unit Effect.t

let debug_loctext t = Effect.perform @@ Debug t
let debug ?loc s = debug_loctext @@ Diagnostic.loctext ?loc s
let debugf ?loc = Diagnostic.kloctextf ?loc debug_loctext

let stalk_open_loctext t = Effect.perform @@ CallBegin t
let stalk_close_loctext t = Effect.perform @@ CallEnd t
let stalk ?loc s f =
  stalk_open_loctext (Diagnostic.loctext ?loc s);
  Fun.protect f
    ~finally:(fun () -> stalk_close_loctext (Diagnostic.loctext ?loc s))
let stalkf ?loc =
  Diagnostic.ktextf @@ fun t f ->
  stalk_open_loctext {Range.value = t; loc};
  Fun.protect f
    ~finally:(fun () -> stalk_close_loctext {Range.value = t; loc})
