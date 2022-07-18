open Asai

let slice_single_line_ascii_string () =
  let filename = "test" in
  let bol = 0 in
  let line = 1 in

  let start = Pos.create ~point:2 ~bol ~line ~filename in
  let stop = Pos.create ~point:5 ~bol ~line ~filename in
  let span = Span.spanning start stop in

  let expected = "bbb" in
  let actual = Span.utf8_slice "aabbbcccccc" span in
  Alcotest.(check string) "Extracted the correct slice" expected actual

let slice_multi_line_ascii_string () =
  let filename = "test" in
  let start = Pos.create ~point:2 ~bol:0 ~line:1 ~filename in
  let stop = Pos.create ~point:6 ~bol:5 ~line:2 ~filename in
  let span = Span.spanning start stop in

  let expected = "b\nbb" in
  let actual = Span.utf8_slice "aab\nbbccc" span in
  Alcotest.(check string) "Extracted the correct slice" expected actual

let slice_lines_multline_ascii_string () =
  let filename = "test" in
  let start = Pos.create ~point:4 ~bol:3 ~line:2 ~filename in
  let stop = Pos.create ~point:9 ~bol:7 ~line:3 ~filename in
  let span = Span.spanning start stop in

  let expected = ("b", "bb\ncc", "cc") in
  let actual = Span.utf8_slice_lines "aa\nbbb\ncccc\nddd" span in
  Alcotest.(check (triple string string string)) "Extracted the correct slice" expected actual

let slice_lines_end_of_string () =
  let filename = "test" in
  let start = Pos.create ~point:4 ~bol:0 ~line:1 ~filename in
  let stop = Pos.create ~point:9 ~bol:0 ~line:1 ~filename in
  let span = Span.spanning start stop in
  let expected = ("aaaa", "bbbbb", "") in
  let actual = Span.utf8_slice_lines "aaaabbbbb" span in
  Alcotest.(check (triple string string string)) "Extracted the correct slice" expected actual

let slice_lines_unicode_string () =
  let filename = "test" in
  let start = Pos.create ~point:2 ~bol:0 ~line:1 ~filename in
  let stop = Pos.create ~point:4 ~bol:0 ~line:1 ~filename in
  let span = Span.spanning start stop in
  let expected = ("λ", "α", "β") in
  let actual = Span.utf8_slice_lines "λαβ" span in
  Alcotest.(check (triple string string string)) "Extracted the correct slice" expected actual

let tests =
  let open Alcotest in [
    test_case "Slice a single line ASCII string" `Quick slice_single_line_ascii_string;
    test_case "Slice a multi line ASCII string" `Quick slice_multi_line_ascii_string;
    test_case "Slice lines of an ASCII string" `Quick slice_lines_multline_ascii_string;
    test_case "Slice till the end of line of an ASCII string" `Quick slice_lines_end_of_string;
    test_case "Slice lines of a UTF8 string" `Quick slice_lines_unicode_string
  ]
