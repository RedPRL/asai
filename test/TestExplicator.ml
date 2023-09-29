open Asai

module FilePathAsContent : Explicator.Reader =
struct
  type file = string
  let load s = s
  let length s = String.length s
  let unsafe_get s i = s.[i]
end

module IntStyle : Explicator.Style with type t = int =
struct
  type t = int
  let default = 0
  let equal = Int.equal
  let compare = Int.compare
  let max = Int.max
  let is_default i = i = 0
  let compose = (+)
  let dump = Format.pp_print_int
end

module E = Explicator.Make(FilePathAsContent)(IntStyle)

let test_explication = Alcotest.of_pp (Explication.dump IntStyle.dump)

let single_line () =
  let file_path = "aabbbcccccc" in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line1 with offset = 5}) in
  let expected : _ Explication.t =
    [ {file_path;
       blocks =
         [{start_line_num = 1;
           lines =
             [[
               {style=0; value="aa"};
               {style=1; value="bbb"};
               {style=0; value="cccccc"};
             ]]}
         ]}
    ] in
  let actual = E.explicate [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let single_line_with_crlf () =
  let file_path = "aabbbcccccc\r\n" in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line1 with offset = 5}) in
  let expected : _ Explication.t =
    [ {file_path;
       blocks =
         [{start_line_num = 1;
           lines =
             [[
               {style=0; value="aa"};
               {style=1; value="bbb"};
               {style=0; value="cccccc"};
             ]]}
         ]}
    ] in
  let actual = E.explicate [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_crlf () =
  let file_path = "aabbbbb\r\nbbbbccc" in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let start_of_line2 : Span.position = {file_path; offset = 9; start_of_line = 9; line_num = 2} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line2 with offset = 13}) in
  let expected : _ Explication.t =
    [ {file_path;
       blocks =
         [{start_line_num = 1;
           lines =
             [[
               {style=0; value="aa"};
               {style=1; value="bbbbb"};
             ];
              [
                {style=1; value="bbbb"};
                {style=0; value="ccc"};
              ]]}
         ]}
    ] in
  let actual = E.explicate ~line_breaking:`Traditional [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let file_path = "aabbbbb\r\nbbbbccc" in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let start_of_line2 : Span.position = {file_path; offset = 9; start_of_line = 9; line_num = 2} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line2 with offset = 13}) in
  let expected : _ Explication.t =
    [ {file_path;
       blocks =
         [{start_line_num = 1;
           lines =
             [[
               {style=0; value="aa"};
               {style=1; value="bbbbb"};
             ];
              [
                {style=1; value="bbbb"};
                {style=0; value="ccc"};
              ]]}
         ]}
    ] in
  let actual = E.explicate ~line_breaking:`Unicode [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

(*
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
*)

let tests =
  let open Alcotest in
  Alcotest.run "Explicator" [
    "Explicator",
    [
      test_case "single-line span" `Quick single_line;
      test_case "single-line span" `Quick single_line_with_crlf;
      test_case "multi-line span with CRLF" `Quick multi_lines_with_crlf;
      test_case "multi-line span with LS" `Quick multi_lines_with_ls;
      (*
      test_case "Slice a multi line ASCII string" `Quick slice_multi_line_ascii_string;
      test_case "Slice lines of an ASCII string" `Quick slice_lines_multline_ascii_string;
      test_case "Slice till the end of line of an ASCII string" `Quick slice_lines_end_of_string;
      test_case "Slice lines of a UTF8 string" `Quick slice_lines_unicode_string
      *)
    ]
  ]
