open Asai

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

module E = Explicator.Make(IntStyle)

let test_explication = Alcotest.of_pp (Explication.dump IntStyle.dump)

let single_line mode eol () =
  let source = `String ("aaabbbcccdddeee" ^ eol) in
  let start_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let span1 = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 3}, {start_of_line1 with offset = 9}) in
  let span2 = Explication.style 2 @@ Span.make ({start_of_line1 with offset = 6}, {start_of_line1 with offset = 12}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{start_line_num = 1;
          lines =
            [[{style=0; value="aaa"};
              {style=1; value="bbb"};
              {style=3; value="ccc"};
              {style=2; value="ddd"};
              {style=0; value="eee"};
             ]]}
        ]}
    ] in
  let actual = E.explicate ~line_breaking:mode [span1; span2] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let source = `String "aabbbbb\u{2028}bbbbccc" in
  let start_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let start_of_line2 : Span.position = {source; offset = 10; start_of_line = 10; line_num = 2} in
  let span = Explication.style 1 @@ Span.make ({start_of_line1 with offset = 2}, {start_of_line2 with offset = 14}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{start_line_num = 1;
          lines =
            [[{style=0; value="aa"};
              {style=1; value="bbbbb"};
             ];
             [{style=1; value="bbbb"};
              {style=0; value="ccc"};
             ]]}
        ]}
    ]
  in
  let actual = E.explicate ~line_breaking:`Unicode [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines () =
  let source = `String
      {|
aabbbbb
bbbbbbb
b*ccddd
1
2
3
4
ee++fff
1
2
3
4
5
ggggghh
|}
  in
  let start_of_line2 : Span.position = {source; offset = 1; start_of_line = 1; line_num = 2} in
  let start_of_line4 : Span.position = {source; offset = 17; start_of_line = 17; line_num = 4} in
  let start_of_line9 : Span.position = {source; offset = 33; start_of_line = 33; line_num = 9} in
  let start_of_line15 : Span.position = {source; offset = 51; start_of_line = 51; line_num = 15} in
  let spans =
    [
      Explication.style 2 @@ Span.make ({start_of_line4 with offset = 17+1}, {start_of_line4 with offset = 17+4});
      Explication.style 1 @@ Span.make ({start_of_line2 with offset = 1+2}, {start_of_line4 with offset = 17+4});
      Explication.style 4 @@ Span.make ({start_of_line9 with offset = 33+2}, {start_of_line9 with offset = 33+7});
      Explication.style 8 @@ Span.make ({start_of_line9 with offset = 33+4}, {start_of_line9 with offset = 33+7});
      Explication.style 16 @@ Span.make (start_of_line15, {start_of_line15 with offset = 51+5});
    ]
  in
  let expected : _ Explication.t =
    [{source;
      blocks=
        [{start_line_num=2;
          lines=
            [[{style=0;value="aa"};
              {style=1;value="bbbbb"}];
             [{style=1;value="bbbbbbb"}];
             [{style=1;value="b"};
              {style=3;value="*cc"};
              {style=0;value="ddd"}];
             [{style=0;value="1"}];
             [{style=0;value="2"}];
             [{style=0;value="3"}];
             [{style=0;value="4"}];
             [{style=0;value="ee"};
              {style=4;value="++"};
              {style=12;value="fff"}]]};
         {start_line_num=15;
          lines=
            [[{style=16;value="ggggg"};
              {style=0;value="hh"}]]}]}]
  in
  let actual = E.explicate ~line_breaking:`Traditional ~block_splitting_threshold:5 spans in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let () =
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
      test_case "multi-line span with CRLF" `Quick multi_lines;
      test_case "multi-line span with LS" `Quick multi_lines_with_ls;
    ]
  ]
