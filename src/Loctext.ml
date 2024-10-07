type t = Text.t Range.located

let make ?loc s : t = Range.locate_opt loc @@ Text.make s

let kmakef ?loc k = Text.kmakef @@ fun t -> k @@ Range.locate_opt loc t

let makef ?loc = Text.kmakef @@ Range.locate_opt loc
