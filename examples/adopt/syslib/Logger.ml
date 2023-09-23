module Code =
struct
  type t = FileError | ChiError | EmojiError

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | _ -> Error

  let to_string =
    function
    | FileError -> "C000"
    | ChiError -> "C001"
    | EmojiError -> "C002"
end

include Asai.Logger.Make(Code)
