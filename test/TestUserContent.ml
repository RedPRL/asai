let of_test name ~tab_size input expected =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check string) "Output matched" expected @@
  Asai.UserContent.replace_control ~tab_size input

let () =
  Alcotest.run "UserContent" [
    "replace_control",
    [
      of_test "tab" ~tab_size:2 "123\t4\t" "123  4  ";
      of_test "C1 control" ~tab_size:3 "\000\r\n" "���";
      of_test "C2 control" ~tab_size:100 "\xc2\x80" "�";
      of_test "invalid UTF-8" ~tab_size:100 "\xc2\xc2" "��";
      of_test "invalid UTF-8" ~tab_size:100 "\xc2\x80\x80" "��";
    ]
  ]
