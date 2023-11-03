type param =
  { enabled : bool
  ; color : bool
  }

type color = [
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
]

type attr = [
  | `Bold
  | `Faint
  | `Underline
  | `Fg of [ color | `Bright of color ]
  | `Bg of [ color | `Bright of color ]
]

type style = attr list

let code_of_attr : attr -> string =
  function
  | `Bold -> "1"
  | `Faint -> "2"
  | `Underline -> "4"
  | `Fg `Black -> "30"
  | `Fg `Red -> "31"
  | `Fg `Green -> "32"
  | `Fg `Yellow -> "33"
  | `Fg `Blue -> "34"
  | `Fg `Magenta -> "35"
  | `Fg `Cyan -> "36"
  | `Fg `White -> "37"
  | `Bg `Black -> "40"
  | `Bg `Red -> "41"
  | `Bg `Green -> "42"
  | `Bg `Yellow -> "43"
  | `Bg `Blue -> "44"
  | `Bg `Magenta -> "45"
  | `Bg `Cyan -> "46"
  | `Bg `White -> "47"
  | `Fg `Bright `Black -> "90"
  | `Fg `Bright `Red -> "91"
  | `Fg `Bright `Green -> "92"
  | `Fg `Bright `Yellow -> "93"
  | `Fg `Bright `Blue -> "94"
  | `Fg `Bright `Magenta -> "95"
  | `Fg `Bright `Cyan -> "96"
  | `Fg `Bright `White -> "97"
  | `Bg `Bright `Black -> "100"
  | `Bg `Bright `Red -> "101"
  | `Bg `Bright `Green -> "102"
  | `Bg `Bright `Yellow -> "103"
  | `Bg `Bright `Blue -> "104"
  | `Bg `Bright `Magenta -> "105"
  | `Bg `Bright `Cyan -> "106"
  | `Bg `Bright `White -> "107"

let not_color : attr -> bool =
  function
  | `Bold | `Faint | `Underline -> true
  | `Fg _ | `Bg _ -> false

let raw_style_string st : string =
  String.concat "" ["\x1b["; String.concat ";" (List.map code_of_attr st); "m"]

let style_string ~param st =
  match param.enabled with
  | false -> ""
  | _ ->
    let st = if param.color then st else List.filter not_color st in
    match st with
    | [] -> ""
    | _ -> raw_style_string st

let reset_string ~param st =
  match param.enabled with
  | false -> ""
  | _ ->
    let st = if param.color then st else List.filter not_color st in
    match st with
    | [] -> ""
    | _ -> raw_style_string []

module Test =
struct
  let no_color =
    match Sys.getenv_opt "NO_COLOR" with
    | None | Some "" -> false
    | _ -> true

  let rich_term =
    match Sys.getenv_opt "TERM" with
    | None | Some "" | Some "dumb" -> false
    | _ -> true

  let is_tty o =
    try Unix.isatty (Unix.descr_of_out_channel o) with _ -> false

  let guess ?use_ansi ?use_color o =
    let enabled = match use_ansi with Some a -> a | None -> rich_term && is_tty o in
    let color = match use_color with Some c -> c | None -> not no_color in
    {enabled; color}
end
