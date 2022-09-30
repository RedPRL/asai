type t =
  [ `TypeError (* Type checking failed *)
  | `UnboundVariable (* Unbound variable *)
  | `RequiresAnnotation (* Unable to infer the type *)
  | `LexingError (* The lexer encountered an error *)
  | `ParsingError (* Parsing errors *)
  ]

let default_severity _ = Asai.Severity.Error

let to_string : t -> string =
  function
  | `TypeError -> "E001"
  | `UnboundVariable -> "E002"
  | `RequiresAnnotation -> "E003"
  | `LexingError -> "E004"
  | `ParsingError -> "E005"
