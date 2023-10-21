open Asai

module E = Explicator.Make(IntTag)

let test_explication = Alcotest.of_pp (Explication.dump IntTag.dump)

let single_line mode eol () =
  let source = `String {Span.title = None; content = "aaabbbcccdddeee" ^ eol} in
  let begin_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let span1 = 1, Span.make ({begin_of_line1 with offset = 3}, {begin_of_line1 with offset = 9}) in
  let span2 = 2, Span.make ({begin_of_line1 with offset = 6}, {begin_of_line1 with offset = 12}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 1;
          lines =
            [{tags = [1; 2];
              segments =
                [(None, "aaa");
                 (Some 1, "bbb");
                 (Some 2, "cccddd");
                 (None, "eee");
                ]}]}
        ]}
    ] in
  let actual = E.explicate ~line_breaking:mode [span1; span2] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let source = `String {Span.title = None; content = "aabbbbb\u{2028}bbbbccc"} in
  let begin_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let begin_of_line2 : Span.position = {source; offset = 10; start_of_line = 10; line_num = 2} in
  let span = 1, Span.make ({begin_of_line1 with offset = 2}, {begin_of_line2 with offset = 14}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 2;
          lines =
            [{tags=[];
              segments=
                [(None, "aa");
                 (Some 1, "bbbbb");
                ]};
             {tags=[1];
              segments=
                [(Some 1, "bbbb");
                 (None, "ccc");
                ]}]}
        ]}
    ]
  in
  let actual = E.explicate ~line_breaking:`Unicode [span] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines () =
  let source =
    `String
      {Span.title = None;
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
  let begin_of_line2 : Span.position = {source; offset = 1; start_of_line = 1; line_num = 2} in
  let begin_of_line4 : Span.position = {source; offset = 17; start_of_line = 17; line_num = 4} in
  let begin_of_line9 : Span.position = {source; offset = 33; start_of_line = 33; line_num = 9} in
  let begin_of_line15 : Span.position = {source; offset = 51; start_of_line = 51; line_num = 15} in
  let spans =
    [
      2, Span.make ({begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4});
      1, Span.make ({begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+4});
      4, Span.make ({begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+7});
      8, Span.make ({begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7});
      16, Span.make (begin_of_line15, {begin_of_line15 with offset = 51+5});
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
                 (Some 1, "bbbbb")]};
             {tags=[];
              segments=
                [(Some 1, "bbbbbbb")]};
             {tags=[1;2];
              segments=
                [(Some 1, "b");
                 (Some 2, "*cc");
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
             {tags=[4; 8];
              segments=
                [(None, "ee");
                 (Some 4, "++");
                 (Some 8, "fff")]}]};
         {begin_line_num=15;
          end_line_num=15;
          lines=
            [{tags=[16];
              segments=
                [(Some 16, "ggggg");
                 (None, "hh")]}]}]}]
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
      test_case "multi-line span with LS" `Quick multi_lines_with_ls;
      test_case "multi-line span with CRLF" `Quick multi_lines;
    ]
  ]
