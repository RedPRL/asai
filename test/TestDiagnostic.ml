let of_test name input expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check string) "Output matched" expected @@
  Asai.Diagnostic.string_of_text input

let () =
  Alcotest.run "UserContent" [
    "string_of_text",
    [
      of_test "\\n" (Asai.Diagnostic.text " \r   e \n  f  \n   g   ") "  e  f   g   ";
      of_test "\\n" (Asai.Diagnostic.textf "@[<v>123@[456@]789@]") "123 456789";
    ]
  ]
