module Message =
struct
  type t =
    | Syslib of Syslib.Reporter.Message.t
    | UserError

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | _ -> Warning

  let short_code : t -> string = function
    | Syslib c -> Syslib.Reporter.Message.short_code c
    | UserError -> "A000"

  let syslib c = Syslib c
end

include Asai.Reporter.Make(Message)

let lift_syslib f = adopt (Asai.Diagnostic.map Message.syslib) Syslib.Reporter.run f

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
