open Asai

module E = Explicator.Make(IntTag)
module F = Flattener.Make(IntTag)

let test_flattened = Alcotest.of_pp (Flattener.dump IntTag.dump)

let single_line_flatten () =
  let source = `String {Span.title = None; content = "aaabbbcccdddeee"} in
  let begin_of_line1 : Span.position = {source; offset = 0; start_of_line = 0; line_num = 1} in
  let pt1, pt2, pt3, pt4 =
    {begin_of_line1 with offset = 3},
    {begin_of_line1 with offset = 6},
    {begin_of_line1 with offset = 9},
    {begin_of_line1 with offset = 12}
  in
  let span1 = 1, Span.make (pt1, pt3) in
  let span2 = 2, Span.make (pt2, pt4) in
  let expected : _ Flattener.t =
    [(source,
      [{begin_line_num=1;
        end_line_num=1;
        tagged_positions=[(Some 1, pt1);(Some 2, pt2);(Some 2, pt3);(None, pt4)];
        tagged_lines=[(1,1);(2,1)]}])]
  in
  let actual = F.flatten ~block_splitting_threshold:5 ~blend:(Explicator.default_blend ~priority:IntTag.priority) [span1; span2] in
  Alcotest.(check test_flattened) "Flattener is correct" expected actual

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
  let pt3, pt18, pt21, pt35, pt37, pt40, pt56 =
    {begin_of_line2 with offset = 1+2}, {begin_of_line4 with offset = 17+1}, {begin_of_line4 with offset = 17+4},
    {begin_of_line9 with offset = 33+2}, {begin_of_line9 with offset = 33+4}, {begin_of_line9 with offset = 33+7},
    {begin_of_line15 with offset = 51+5}
  in
  let spans =
    [
      2, Span.make (pt18, pt21);
      1, Span.make (pt3, pt21);
      4, Span.make (pt35, pt40);
      8, Span.make (pt37, pt40);
      16, Span.make (begin_of_line15, pt56);
    ]
  in
  let expected : _ Flattener.t =
    [(source,
      [{begin_line_num=2;
        end_line_num=9;
        tagged_positions=
          [(Some 1, pt3);
           (Some 2, pt18);
           (None, pt21);
           (Some 4, pt35);
           (Some 8, pt37);
           (None, pt40)];
        tagged_lines=
          [(1, 4);
           (2, 4);
           (4, 9);
           (8, 9)]};
       {begin_line_num=15;
        end_line_num=15;
        tagged_positions=
          [(Some 16, begin_of_line15);
           (None, pt56)];
        tagged_lines=
          [(16, 15)]}])]
  in
  let actual = F.flatten ~block_splitting_threshold:5 ~blend:(Explicator.default_blend ~priority:IntTag.priority) spans in
  Alcotest.(check test_flattened) "Flattener is correct" expected actual

let () =
  let open Alcotest in
  Alcotest.run "Flattener" [
    "flattening",
    [
      test_case "single-line spans" `Quick single_line_flatten;
      test_case "multi-line spans" `Quick multi_lines;
    ]
  ]
