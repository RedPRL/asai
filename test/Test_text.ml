let of_test name input expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check string) "Output matched" expected @@
  Asai.Text.to_string input

let () =
  Alcotest.run "UserContent" [
    "Text.to_string",
    [
      of_test "\\n" (Asai.Text.make " \r   e \n  f  \n   g   ") "  e  f   g   ";
      of_test "\\n" (Asai.Text.makef "@[<v>123@[456@]789@]") "123 456789";
    ]
  ]
