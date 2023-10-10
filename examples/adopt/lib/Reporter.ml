module Message =
struct
  type t = FileError | ChiError | EmojiError

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | _ -> Error

  let short_code =
    function
    | FileError -> "C000"
    | ChiError -> "C001"
    | EmojiError -> "C002"
end

include Asai.Reporter.Make(Message)
