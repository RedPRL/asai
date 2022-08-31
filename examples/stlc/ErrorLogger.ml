open Asai.Severity


type code =
  | TypeError
  | UnboundVariable
  | RequiresAnnotation
  | LexerError
  | ParseError

type phase =
  | Elaboration
  | Conversion

module Code : Asai.Code.S with type t = code =
struct
  type t = code 

  let default_severity =
    function
    | TypeError -> Error
    | UnboundVariable -> Error
    | RequiresAnnotation -> Error
    | LexerError -> Error
    | ParseError -> Error
  let to_string = 
    function
    | TypeError ->
      "The typechecker encountered a type error."
    | UnboundVariable ->
      "We encountered a variable that was not in scope!"
    | RequiresAnnotation ->
      "We were unable to infer the type of some term."
    | LexerError ->
      "The lexer encountered an error."
    | ParseError ->
      "The parser encountered an error."
end

module Phase : Asai.Phase.S with type t = phase = 
struct
  type t = phase

  let to_string = function
    | Elaboration -> "Elaboration"
    | Conversion -> "Conversion"
end

include Asai.Logger.Make(Code)(Phase)
