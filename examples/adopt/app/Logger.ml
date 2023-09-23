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
end

include Asai.Logger.Make(Code)

let embed_syslib d = Asai.Diagnostic.map (fun c -> Code.Syslib c) d
let lift_syslib f = adopt embed_syslib Syslib.Logger.run f
