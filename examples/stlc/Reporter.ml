module Message =
struct
  type t =
    | TypeError (* Type checking failed *)
    | UnboundVariable (* Unbound variable *)
    | RequiresAnnotation (* Unable to infer the type *)
    | ParsingError (* Parsing errors *)

  let default_severity _ = Asai.Diagnostic.Error

  let short_code : t -> string =
    function
    | TypeError -> "E001"
    | UnboundVariable -> "E002"
    | RequiresAnnotation -> "E003"
    | ParsingError -> "E004"
end

include Asai.Reporter.Make(Message)
