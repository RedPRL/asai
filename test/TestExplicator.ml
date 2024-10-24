open Asai

module E = Explicator.Make(IntTag)

let test_explication = Alcotest.of_pp (Explication.dump IntTag.dump)

let single_line mode eol () =
  let source = `String {Range.title = None; content = "aaabbbcccdddeee" ^ eol} in
  let begin_of_line1 : Range.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let range1 = (1, "1"), Range.make ({begin_of_line1 with offset = 3}, {begin_of_line1 with offset = 9}) in
  let range2 = (2, "2"), Range.make ({begin_of_line1 with offset = 6}, {begin_of_line1 with offset = 12}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 1;
          lines =
            [{tags = [(1, "1"); (2, "2")];
              segments =
                [(None, "aaa");
                 (Some (1, "1"), "bbb");
                 (Some (2, "2"), "ccc");
                 (Some (2, "2"), "ddd");
                 (None, "eee");
                ]}]}
        ]}
    ] in
  let actual = E.explicate ~line_breaks:mode [range1; range2] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let source = `String {Range.title = None; content = "aabbbbb\u{2028}bbbbccc"} in
  let begin_of_line1 : Range.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let begin_of_line2 : Range.position = {source; offset = 10; start_of_line = 10; line_num = 2} in
  let range = (1, "1"), Range.make ({begin_of_line1 with offset = 2}, {begin_of_line2 with offset = 14}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 2;
          lines =
            [{tags=[];
              segments=
                [(None, "aa");
                 (Some (1, "1"), "bbbbb");
                ]};
             {tags=[(1, "1")];
              segments=
                [(Some (1, "1"), "bbbb");
                 (None, "ccc");
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
      (2, "1"), Range.make ({begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4});
      (1, "2"), Range.make ({begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+4});
      (4, "3"), Range.make ({begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+7});
      (8, "4"), Range.make ({begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7});
      (16, "5"), Range.make (begin_of_line15, {begin_of_line15 with offset = 51+5});
    ]
  in
  let expected : _ Explication.t =
    [{source;
      blocks=
        [{begin_line_num=2;
          end_line_num=9;
          lines=
            [{tags=[];
              segments=
                [(None, "aa");
                 (Some (1, "2"), "bbbbb")]};
             {tags=[];
              segments=
                [(Some (1, "2"), "bbbbbbb")]};
             {tags=[(1, "2"); (2, "1")];
              segments=
                [(Some (1, "2"), "b");
                 (Some (2, "1"), "*cc");
                 (None, "ddd")]};
             {tags=[];
              segments=
                [(None, "1")]};
             {tags=[];
              segments=
                [(None, "2")]};
             {tags=[];
              segments=
                [(None, "3")]};
             {tags=[];
              segments=
                [(None, "4")]};
             {tags=[(4, "3"); (8, "4")];
              segments=
                [(None, "ee");
                 (Some (4, "3"), "++");
                 (Some (8, "4"), "fff")]}]};
         {begin_line_num=15;
          end_line_num=15;
          lines=
            [{tags=[(16, "5")];
              segments=
                [(Some (16, "5"), "ggggg");
                 (None, "hh")]}]}]}]
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
