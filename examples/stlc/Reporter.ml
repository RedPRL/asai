module Message =
struct
  type t =
    | Type_error (* Type checking failed *)
    | Unbound_variable (* Unbound variable *)
    | Requires_annotation (* Unable to infer the type *)
    | Parsing_error (* Parsing errors *)

  let default_severity _ = Asai.Diagnostic.Error

  let short_code : t -> string =
    function
    | Type_error -> "E001"
    | Unbound_variable -> "E002"
    | Requires_annotation -> "E003"
    | Parsing_error -> "E004"
end

include Asai.Reporter.Make(Message)
