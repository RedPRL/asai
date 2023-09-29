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
    "Explicator",
    [
      test_case "single-line span" `Quick single_line;
      test_case "single-line span" `Quick single_line_with_crlf;
      test_case "multi-line span with CRLF" `Quick multi_lines_with_crlf;
      test_case "multi-line span with LS" `Quick multi_lines_with_ls;
    ]
  ]
