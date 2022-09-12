(** Severity levels *)
type t =
  | Info
  | Warning
  | Error
  | InternalError

let pp fmt =
  function
  | Info -> Format.fprintf fmt "Info"
  | Warning -> Format.fprintf fmt "Warning"
  | Error -> Format.fprintf fmt "Error"
  | InternalError -> Format.fprintf fmt "Internal Error"

let pp_short fmt =
  function
  | Info -> Format.fprintf fmt "I"
  | Warning -> Format.fprintf fmt "W"
  | Error -> Format.fprintf fmt "E"
  | InternalError -> Format.fprintf fmt "X"
