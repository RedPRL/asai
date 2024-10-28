let code ~param (severity : Diagnostic.severity) : Ansi.style =
  match param with
  | `Disabled -> []
  | `Enabled_without_color -> [`Bold]
  | `Enabled_with_color ->
    match severity with
    | Hint -> [`Fg `Blue]
    | Info -> [`Fg `Green]
    | Warning -> [`Fg `Yellow]
    | Error -> [`Fg `Red]
    | Bug -> [`Bg `Red; `Fg `Black]

let message ~param (severity : Diagnostic.severity) (tag : Tty_tag.t) : Ansi.style =
  match tag with
  | `Main_message, _ -> code ~param severity
  | `Extra_remark _, _ -> []

let highlight ~param (severity : Diagnostic.severity) : Tty_tag.t option -> Ansi.style =
  function
  | None -> []
  | Some tag -> [`Underline] @ message ~param severity tag

let fringe = [`Faint]

let indentation = [`Faint]
