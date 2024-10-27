open Asai

module E = Explicator.Make(IntTag)
module F = Flattener.Make(IntTag)

let test_flattened = Alcotest.of_pp (Flattener.dump IntTag.dump)

let single_line_flatten () =
  let source = `String {Range.title = None; content = "aaabbbcccdddeee"} in
  let begin_of_line1 : Range.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let pt1, pt2, pt3, pt4 =
    {begin_of_line1 with offset = 3},
    {begin_of_line1 with offset = 6},
    {begin_of_line1 with offset = 9},
    {begin_of_line1 with offset = 12}
  in
  let ranges =
    [ Range.make (pt1, pt3), (1, "1")
    ; Range.make (pt1, pt3), (2, "2")
    ; Range.make (pt1, pt3), (1, "3")
    ; Range.make (pt2, pt4), (3, "4")
    ]
  in
  let expected : _ Flattener.t =
    [(source,
      [{begin_line_num=1;
        end_line_num=1;
        markers=
          [ pt1, RangeBegin (1, "3")
          ; pt1, RangeBegin (1, "1")
          ; pt1, RangeBegin (2, "2")
          ; pt2, RangeBegin (3, "4")
          ; pt3, RangeEnd (2, "2")
          ; pt3, RangeEnd (1, "1")
          ; pt3, RangeEnd (1, "3")
          ; pt4, RangeEnd (3, "4")
          ];
        line_markers=[(1, (2, "2")); (1, (1, "1")); (1, (1, "3")); (1, (3, "4"))]}])]
  in
  let actual = F.flatten ~block_splitting_threshold:5 ranges in
  Alcotest.(check test_flattened) "Flattener is correct" expected actual

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
  let pt3, pt18, pt21, pt35, pt37, pt40, pt56 =
    {begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4},
    {begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7},
    {begin_of_line15 with offset = 51+5}
  in
  let ranges =
    [
      Range.make (pt18, pt21), (2, "1");
      Range.make (pt3, pt21), (1, "2");
      Range.make (pt35, pt40), (4, "3");
      Range.make (pt37, pt40), (8, "4");
      Range.make (begin_of_line15, pt56), (16, "5");
    ]
  in
  let expected : _ Flattener.t =
    [(source,
      [{begin_line_num=2;
        end_line_num=9;
        markers=
          [(pt3, RangeBegin (1, "2"));
           (pt18, RangeBegin (2, "1"));
           (pt21, RangeEnd (2, "1"));
           (pt21, RangeEnd (1, "2"));
           (pt35, RangeBegin (4, "3"));
           (pt37, RangeBegin (8, "4"));
           (pt40, RangeEnd (8, "4"));
           (pt40, RangeEnd (4, "3"));
          ];
        line_markers=
          [ 4, (2, "1")
          ; 4, (1, "2")
          ; 9, (8, "4")
          ; 9, (4, "3")
          ]};
       {begin_line_num=15;
        end_line_num=15;
        markers=
          [(begin_of_line15, RangeBegin (16, "5"));
           (pt56, RangeEnd (16, "5"))];
        line_markers=
          [(15, (16, "5"))]}])]
  in
  let actual = F.flatten ~block_splitting_threshold:5 ranges in
  Alcotest.(check test_flattened) "Flattener is correct" expected actual

let () =
  let open Alcotest in
  Alcotest.run "Flattener" [
    "flattening",
    [
      test_case "single-line ranges" `Quick single_line_flatten;
      test_case "multi-line ranges" `Quick multi_lines;
    ]
  ]
