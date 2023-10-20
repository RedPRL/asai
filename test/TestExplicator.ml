open Asai

module IntTag : Explicator.Tag with type t = int =
struct
  type t = int
  let equal = Int.equal
  let priority i = -i
  let compose = (+)
  let dump = Format.pp_print_int
end

module E = Explicator.Make(IntTag)

let test_explication = Alcotest.of_pp (Explication.dump IntTag.dump)

let single_line mode eol () =
  let source = `String {Span.title = None; content = "aaabbbcccdddeee" ^ eol} in
  let begin_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let span1 = Explication.tag 1 @@ Span.make ({begin_of_line1 with offset = 3}, {begin_of_line1 with offset = 9}) in
  let span2 = Explication.tag 2 @@ Span.make ({begin_of_line1 with offset = 6}, {begin_of_line1 with offset = 12}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          end_line_num = 1;
          lines =
            [{tags = [];
              segments =
                [{tag=None; value="aaa"};
                 {tag=Some 1; value="bbb"};
                 {tag=Some 3; value="ccc"};
                 {tag=Some 2; value="ddd"};
                 {tag=None; value="eee"};
                ]}]}
        ]}
    ] in
  let actual = E.explicate ~line_breaking:mode [span1; span2] in
  Alcotest.(check test_explication) "Explication is correct" expected actual

let multi_lines_with_ls () =
  let source = `String {Span.title = None; content = "aabbbbb\u{2028}bbbbccc"} in
  let begin_of_line1 : Span.position = {source; offset = 0; begin_of_line = 0; line_num = 1} in
  let begin_of_line2 : Span.position = {source; offset = 10; begin_of_line = 10; line_num = 2} in
  let span = Explication.tag 1 @@ Span.make ({begin_of_line1 with offset = 2}, {begin_of_line2 with offset = 14}) in
  let expected : _ Explication.t =
    [{source;
      blocks =
        [{begin_line_num = 1;
          lines =
            [[{tag=0; value="aa"};
              {tag=1; value="bbbbb"};
             ];
             [{tag=1; value="bbbb"};
              {tag=0; value="ccc"};
             ]]}
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
  let begin_of_line2 : Span.position = {source; offset = 1; begin_of_line = 1; line_num = 2} in
  let begin_of_line4 : Span.position = {source; offset = 17; begin_of_line = 17; line_num = 4} in
  let begin_of_line9 : Span.position = {source; offset = 33; begin_of_line = 33; line_num = 9} in
  let begin_of_line15 : Span.position = {source; offset = 51; begin_of_line = 51; line_num = 15} in
  let spans =
    [
      Explication.tag 2 @@ Span.make ({begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4});
      Explication.tag 1 @@ Span.make ({begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+4});
      Explication.tag 4 @@ Span.make ({begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+7});
      Explication.tag 8 @@ Span.make ({begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7});
      Explication.tag 16 @@ Span.make (begin_of_line15, {begin_of_line15 with offset = 51+5});
    ]
  in
  let expected : _ Explication.t =
    [{source;
      blocks=
        [{begin_line_num=2;
          lines=
            [[{tag=0;value="aa"};
              {tag=1;value="bbbbb"}];
             [{tag=1;value="bbbbbbb"}];
             [{tag=1;value="b"};
              {tag=3;value="*cc"};
              {tag=0;value="ddd"}];
             [{tag=0;value="1"}];
             [{tag=0;value="2"}];
             [{tag=0;value="3"}];
             [{tag=0;value="4"}];
             [{tag=0;value="ee"};
              {tag=4;value="++"};
              {tag=12;value="fff"}]]};
         {begin_line_num=15;
          lines=
            [[{tag=16;value="ggggg"};
              {tag=0;value="hh"}]]}]}]
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
