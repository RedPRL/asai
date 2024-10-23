open Asai

module E = Explicator.Make(IntTag)

let test_explication = Alcotest.of_pp (Explication.dump IntTag.dump)

let single_line mode eol () =
  let source = `String {Range.title = None; content = "aaabbbcccdddeee" ^ eol} in
  let begin_of_line1 : Range.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let range1 = Range.make ({begin_of_line1 with offset = 3}, {begin_of_line1 with offset = 9}), (1, "1") in
  let range2 = Range.make ({begin_of_line1 with offset = 6}, {begin_of_line1 with offset = 12}), (2, "2") in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 1;
          lines =
            [{markers = [(1, "1"); (2, "2")];
              tokens =
                [String "aaa";
                 Marker (RangeBegin (1, "1"));
                 String "bbb";
                 Marker (RangeBegin (2, "2"));
                 String "ccc";
                 Marker (RangeEnd (1, "1"));
                 String "ddd";
                 Marker (RangeEnd (2, "2"));
                 String "eee";
                ]}]}
        ]}
    ] in
  let actual = E.explicate ~line_breaks:mode [range1; range2] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let source = `String {Range.title = None; content = "aabbbbb\u{2028}bbbbccc"} in
  let begin_of_line1 : Range.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let begin_of_line2 : Range.position = {source; offset = 10; start_of_line = 10; line_num = 2} in
  let range = Range.make ({begin_of_line1 with offset = 2}, {begin_of_line2 with offset = 14}), (1, "1") in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 2;
          lines =
            [{markers=[];
              tokens=
                [String "aa";
                 Marker (RangeBegin (1, "1"));
                 String "bbbbb";
                ]};
             {markers=[(1, "1")];
              tokens=
                [String "bbbb";
                 Marker (RangeEnd (1, "1"));
                 String "ccc";
                ]}]}
        ]}
    ]
  in
  let actual = E.explicate ~line_breaks:`Unicode [range] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines () =
  let source =
    `String
      {Range.title = None;
       content =
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
|}}
  in
  let begin_of_line2 : Range.position = {source; offset = 1; start_of_line = 1; line_num = 2} in
  let begin_of_line4 : Range.position = {source; offset = 17; start_of_line = 17; line_num = 4} in
  let begin_of_line9 : Range.position = {source; offset = 33; start_of_line = 33; line_num = 9} in
  let begin_of_line15 : Range.position = {source; offset = 51; start_of_line = 51; line_num = 15} in
  let ranges =
    [
      Range.make ({begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4}), (2, "1");
      Range.make ({begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+4}), (1, "2");
      Range.make ({begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+7}), (4, "3");
      Range.make ({begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7}), (8, "4");
      Range.make (begin_of_line15, {begin_of_line15 with offset = 51+5}), (16, "5");
    ]
  in
  let expected : _ Explication.t =
    [{source;
      blocks=
        [{begin_line_num=2;
          end_line_num=9;
          lines=
            [{markers=[];
              tokens=
                [String "aa";
                 Marker (RangeBegin (1, "2"));
                 String "bbbbb"]};
             {markers=[];
              tokens=
                [String "bbbbbbb"]};
             {markers=[(2, "1"); (1, "2")];
              tokens=
                [String "b";
                 Marker (RangeBegin (2, "1"));
                 String "*cc";
                 Marker (RangeEnd (2, "1"));
                 Marker (RangeEnd (1, "2"));
                 String "ddd"]};
             {markers=[];
              tokens=
                [String "1"]};
             {markers=[];
              tokens=
                [String "2"]};
             {markers=[];
              tokens=
                [String "3"]};
             {markers=[];
              tokens=
                [String "4"]};
             {markers=[(8, "4"); (4, "3")];
              tokens=
                [String "ee";
                 Marker (RangeBegin (4, "3"));
                 String "++";
                 Marker (RangeBegin (8, "4"));
                 String "fff";
                 Marker (RangeEnd (8, "4"));
                 Marker (RangeEnd (4, "3"))]}]};
         {begin_line_num=15;
          end_line_num=15;
          lines=
            [{markers=[(16, "5")];
              tokens=
                [Marker (RangeBegin (16, "5"));
                 String "ggggg";
                 Marker (RangeEnd (16, "5"));
                 String "hh"]}]}]}]
  in
  let actual = E.explicate ~line_breaks:`Traditional ~block_splitting_threshold:5 ranges in
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
      test_case "multi-line range with LS" `Quick multi_lines_with_ls;
      test_case "multi-line range with CRLF" `Quick multi_lines;
    ]
  ]
