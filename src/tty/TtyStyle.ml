let code ~param (severity : Diagnostic.severity) : Ansi.style =
  if not param.Ansi.color then
    [`Bold]
  else
    match severity with
    | Hint -> [`Fg `Blue]
    | Info -> [`Fg `Green]
    | Warning -> [`Fg `Yellow]
    | Error -> [`Fg `Red]
    | Bug -> [`Bg `Red; `Fg `Black]

let message ~param (severity : Diagnostic.severity) (tag : TtyTag.t) : Ansi.style =
  match tag with
  | Extra _, _ -> []
  | Main, _ -> code ~param severity

let highlight ~param (severity : Diagnostic.severity) : TtyTag.t option -> Ansi.style =
  function
  | None -> []
  | Some tag -> [`Underline] @ message ~param severity tag

let fringe ~param:_ = [`Faint]

let indentation ~param:_ = [`Faint]
