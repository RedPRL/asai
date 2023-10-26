open Notty
open Notty.Infix

let no_color_lazy = lazy begin
  match Sys.getenv_opt "NO_COLOR" with
  | None | Some "" -> false
  | _ -> true
end

let no_color () = Lazy.force no_color_lazy

let code (severity : Diagnostic.severity) : attr =
  if no_color () then
    A.st A.bold
  else
    match severity with
    | Hint -> A.fg A.blue
    | Info -> A.fg A.green
    | Warning -> A.fg A.yellow
    | Error -> A.fg A.red
    | Bug -> A.bg A.red ++ A.fg A.black

let message (severity : Diagnostic.severity) (tag : TtyTag.t) : attr =
  match tag with
  | Extra _, _ -> A.empty
  | Main, _ -> code severity

let highlight (severity : Diagnostic.severity) : TtyTag.t option -> attr =
  function
  | None -> A.empty
  | Some tag -> A.st A.underline ++ message severity tag

let fringe = if no_color () then A.empty else A.fg @@ A.gray 8

let indentation = if no_color () then A.empty else A.fg @@ A.gray 8
