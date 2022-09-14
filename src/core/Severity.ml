(** Severity levels *)
type t =
  | Hint
  | Info
  | Warning
  | Error
  | Bug

let to_string =
  function
  | Hint -> "Hint"
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
  | Bug -> "Bug"
