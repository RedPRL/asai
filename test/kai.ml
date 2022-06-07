module StringMap = Map.Make(String)

open Kai.Loc

module ErrorCode =
struct
  type t
    = Info0
    | Error1

  let severity =
    function
    | Info0 -> Kai.Severity.Info
    | Error1 -> Kai.Severity.Error

  let code_num =
    function
    | Info0 -> 0
    | Error1 -> 1

  let description =
    function
    | Info0 -> "Some info"
    | Error1 -> "Some error"
end

module Diagnostic = Kai.Diagnostic.Make(ErrorCode)

let test =
  let src = "λ x → x + y" in
  let file_contents = 
    StringMap.of_seq @@ List.to_seq [
      "file.cooltt", src
    ]
  in
  let diag =
    Diagnostic.build ~code:ErrorCode.Error1 "An error happened! Oh no"
    |> Diagnostic.with_cause ~location:(Span.entire src "file.cooltt")
    |> Diagnostic.with_highlight ~location:(Span.relative src 1 6 1 10 "file.cooltt") ~message:"you did something bad!"
  in
  Format.printf "%a"
    (Diagnostic.pp file_contents)
    diag
