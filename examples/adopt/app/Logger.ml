module Code =
struct
  type t =
    | Syslib of Syslib.Logger.Code.t
    | UserError

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | _ -> Warning

  let to_string : t -> string = function
    | Syslib c -> Syslib.Logger.Code.to_string c
    | UserError -> "A000"

  let syslib c = Syslib c
end

include Asai.Logger.Make(Code)

let lift_syslib f = adopt (Asai.Diagnostic.map Code.syslib) Syslib.Logger.run f

let all_as_errors f =
  try_with
    ~emit:(fun d -> emit_diagnostic {d with severity = Error})
    ~fatal:(fun d -> fatal_diagnostic {d with severity = Error})
    f

let abort_at_any f =
  try_with
    ~emit:(fun d -> fatal_diagnostic {d with severity = Error})
    ~fatal:(fun d -> fatal_diagnostic {d with severity = Error})
    f
