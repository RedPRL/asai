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

let single_line mode eol () =
  let file_path = "aaabbbcccdddeee" ^ eol in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let span1 = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 3}, {start_of_line1 with offset = 9}) in
  let span2 = Explication.style 2 @@ Span.make ({start_of_line1 with offset = 6}, {start_of_line1 with offset = 12}) in
  let expected : _ Explication.t =
    [ {file_path;
       blocks =
         [{start_line_num = 1;
           lines =
             [[
               {style=0; value="aaa"};
               {style=1; value="bbb"};
               {style=3; value="ccc"};
               {style=2; value="ddd"};
               {style=0; value="eee"};
             ]]}
         ]}
    ] in
  let actual = E.explicate ~line_breaking:mode [span1; span2] in
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
  let file_path = "aabbbbb\u{2028}bbbbccc" in
  let start_of_line1 : Span.position = {file_path; offset = 0; start_of_line = 0; line_num = 1} in
  let start_of_line2 : Span.position = {file_path; offset = 10; start_of_line = 10; line_num = 2} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line2 with offset = 14}) in
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

let tests =
  let open Alcotest in
  Alcotest.run "Explicator" [
    "single-line",
    [
      test_case "traditional, empty" `Quick (single_line `Traditional "");
      test_case "traditional, CR" `Quick (single_line `Traditional "\r");
      test_case "traditional, LF" `Quick (single_line `Traditional "\n");
      test_case "traditional, CRLF" `Quick (single_line `Traditional "\r\n");
      test_case "unicode, empty" `Quick (single_line `Unicode "");
      test_case "unicode, CR" `Quick (single_line `Unicode "\r");
      test_case "unicode, LF" `Quick (single_line `Unicode "\n");
      test_case "unicode, CRLF" `Quick (single_line `Unicode "\r\n");
      test_case "unicode, VT" `Quick (single_line `Unicode "\x0b");
      test_case "unicode, FF" `Quick (single_line `Unicode "\x0c");
      test_case "unicode, NEL" `Quick (single_line `Unicode "\u{0085}");
      test_case "unicode, LS" `Quick (single_line `Unicode "\u{2028}");
      test_case "unicode, PS" `Quick (single_line `Unicode "\u{2029}");
    ];
    "multi-line",
    [
      test_case "multi-line span with CRLF" `Quick multi_lines_with_crlf;
      test_case "multi-line span with LS" `Quick multi_lines_with_ls;
    ]
  ]
