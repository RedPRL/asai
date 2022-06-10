open Asai.Severity

module ErrorCode =
struct
  type t =
    | TypeError
    | UnboundVariable
    | RequiresAnnotation
    | LexerError
    | ParseError

  let severity =
    function
    | TypeError -> Error
    | UnboundVariable -> Error
    | RequiresAnnotation -> Error
    | LexerError -> Error
    | ParseError -> Error

  let code_num =
    function
    | TypeError -> 1
    | UnboundVariable -> 2
    | RequiresAnnotation -> 3
    | LexerError -> 4
    | ParseError -> 4

  let description = 
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

include Asai.Effects.Make(ErrorCode)
