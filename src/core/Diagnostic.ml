include DiagnosticData

let string_of_severity =
  function
  | Hint -> "Hint"
  | Info -> "Info"
  | Warning -> "Warning"
  | Error -> "Error"
  | Bug -> "Bug"

let map f d = {d with code = f d.code}

let append_marks d marks = { d with additional_marks = d.additional_marks @ marks }
