open Bwd
open Notty
open Notty.Infix

module Style = struct
  type t = HighlightedPrimary | HighlightedAdditional | Primary | Additional | None
  let default = None
  let is_default x = x = None
  let equal (x : t) y = x = y
  let compare (x : t) (y : t) = - Stdlib.compare x y
  let max x y = Stdlib.min x y
  let compose x y = max x y
  let to_string =
    function
    | HighlightedPrimary -> "HighlightedPrimary"
    | HighlightedAdditional -> "HighlightedAdditional"
    | Primary -> "Primary"
    | Additional -> "Additional"
    | None -> "None"
  let dump fmt x = Format.pp_print_string fmt (to_string x)
end

module E = Explicator.Make(FileReader)(Style)

module Make (Code : Diagnostic.Code) =
struct
(*
 ╭
 │    🭁 examples/stlc/example.lambda
 │    │
 │  1 │ (check (λ ä (λ 123
 │  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
 │    ┊
 │ 20 │ ahhhhhhhhhhhhhhhhhh
 │ 21 │ noooooooooooooooooo
 │    ┷
 │ When blah blah blah
 │
 │    🭁 examples/stlc/example.lambda
 │    │
 │  1 │ (check (λ ä (λ 123
 │  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
 │    ┊
 │ 20 │ ahhhhhhhhhhhhhhhhhh
 │ 21 │ noooooooooooooooooo
 │    ┷
 │ When blah blah blah
 ╰
    🭁 examples/stlc/example.lambda
    │
  1 │ (check (λ ä (λ 123
  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
    ┊
 20 │ ahhhhhhhhhhhhhhhhhh
 21 │ noooooooooooooooooo
    ┷
    🭁 examples/stlc/example2.lambda
    │
  3 │ let x = 1
  4 │ let y = 1
    ┷
    🭁 examples/stlc/example3.lambda
    │
  8 │ assert (asai is cool)
    ┷
 [E002] Why am I checking the term (→ ℕ (→ ℕ ℕ))
        which looks amazing!!!

*)

  (* helper functions *)

  let hcat_with_pad ~pad l =
    I.hcat @@ List.map (I.pad ~l:pad) l

  (* styles *)

  let highlight_style (severity : Diagnostic.severity) (style : Style.t) =
    let open A in
    match style with
    | None -> empty
    | HighlightedAdditional | Additional -> st underline
    | HighlightedPrimary | Primary ->
      st underline ++
      match severity with
      | Hint -> fg blue
      | Info -> fg green
      | Warning -> fg yellow
      | Error -> fg red
      | Bug -> bg red ++ fg black

  let fringe_style = A.fg @@ A.gray 8

  (* calculating the width of line numbers *)

  let line_number_width explication : int =
    let max_line_number_block Explication.{start_line_num; lines} =
      start_line_num + List.length lines - 1
    in
    let max_line_number_part Explication.{blocks; _} =
      Utils.maximum @@ List.map max_line_number_block blocks
    in
    let max_line_number (parts : _ Explication.t) =
      Utils.maximum @@ List.map max_line_number_part parts
    in
    String.length @@ Int.to_string @@ max_line_number explication

  (* different parts of the display *)

  (* parameters *)
  type param =
    {
      show_backtrace : bool;
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      line_number_width : int;
      tab_size : int;
      severity : Diagnostic.severity;
      code : Code.t
    }

  (* [ 🭁 examples/stlc/source.lambda] *)
  let render_file_header ~param file_path =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "🭁"
      ; I.string A.empty file_path
      ]

  (* [ │ ] *)
  let render_file_header_padding ~param =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "│"
      ]

  let show_code_segment ~param Explication.{style; value = seg} =
    I.string (highlight_style param.severity style) (UserContent.replace_control ~tab_size:param.tab_size seg)

  let show_code_line ~param (segs : _ Explication.line) =
    I.hcat @@ List.map (show_code_segment ~param) segs

  (* [3 ⇃ no, it is not my fault!!!] *)
  let render_block ~param Explication.{start_line_num; lines} =
    I.vcat @@
    List.mapi (fun i line ->
        let line_num = start_line_num + i in
        hcat_with_pad ~pad:1
          [ I.hsnap ~align:`Right param.line_number_width (I.string fringe_style (Int.to_string line_num))
          ; I.string fringe_style "│"
          ; show_code_line ~param line
          ]
      ) lines

  (* [ ┊ ] *)
  let render_code_block_sep ~param =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "┊"
      ]

  (* [ ┷ ] *)
  let render_code_part_end ~param =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "┷"
      ]

  let render_part ~param Explication.{file_path; blocks} =
    I.vcat @@ List.concat @@
    [ [ render_file_header ~param file_path ]
    ; [ render_file_header_padding ~param ]
    ; begin
      blocks |> List.mapi @@ fun i b ->
      (if i > 0 then render_code_block_sep ~param else I.empty)
      <->
      render_block ~param b
    end
    ; [ render_code_part_end ~param ]
    ]

  let render_explication ~param parts =
    I.vcat @@ List.map (render_part ~param) parts

  (* message *)
  let render_text ~param ~show_code text =
    hcat_with_pad ~pad:1 @@ List.concat
      [ if show_code then [ I.strf "[%s]" (Code.to_string param.code) ] else []
      ; [ I.strf "%t" text ]
      ]

  let render_message ~param ~show_code explication text =
    render_explication ~param explication
    <->
    render_text ~param ~show_code text

  let display_message ~param ~show_code (msg : Diagnostic.message) ~additional_messages =
    let explication =
      let style s x = Explication.{value = x; style = s} in
      let main_span = Option.to_list @@ Option.map (style Style.HighlightedPrimary) msg.loc in
      let additional_spans = List.filter_map (fun x -> Option.map (style Style.Additional) x.Span.loc) additional_messages in
      E.explicate ~block_splitting_threshold:param.block_splitting_threshold (main_span @ additional_spans)
    in
    let line_number_width = Int.max param.line_number_width (line_number_width explication) in
    render_message ~param:{param with line_number_width} ~show_code explication msg.value

  let display_backtrace ~param backtrace =
    let indentation_style = A.fg @@ A.gray 8 in
    let backtrace =
      Bwd.to_list @@ Bwd.map (display_message ~param ~show_code:false ~additional_messages:[]) backtrace
    in
    let backtrace =
      I.vcat @@
      List.mapi
        (fun i image -> (if i > 0 then I.void 0 1 else I.empty) <-> image)
        backtrace
    in
    I.vcat
      [ I.string indentation_style " ╭"
      ; I.tabulate 1 (I.height backtrace) (fun _ _ -> I.string indentation_style " │") <|> backtrace
      ; I.string indentation_style " ╰"
      ]

  let display_diagnostic ~param ~message ~backtrace ~additional_messages =
    FileReader.run @@ fun () ->
    (if param.show_backtrace then display_backtrace ~param backtrace else I.empty)
    <->
    display_message ~param ~show_code:true message ~additional_messages

  module F = Explicator.Make(FileReader)

  let display ?(show_backtrace = false) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; code; message; backtrace; additional_messages} =
    let param = {show_backtrace; line_breaking; block_splitting_threshold; tab_size; severity; code; line_number_width = 2} in
    Notty_unix.output_image @@ Notty_unix.eol @@ display_diagnostic ~param ~message ~backtrace ~additional_messages

  let interactive_trace ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{code; severity; message; additional_messages; backtrace} =
    let param = {show_backtrace = true; line_breaking; block_splitting_threshold; tab_size; severity; code; line_number_width = 2} in
    let traces =
      FileReader.run @@ fun () ->
      Bwd.snoc
        (backtrace |> Bwd.map (fun msg -> display_message ~param ~show_code:true msg ~additional_messages:[]))
        (display_message ~param ~show_code:true message ~additional_messages)
      |> Bwd.to_list |> Array.of_list
    in
    let len = Array.length traces in
    let open Notty_unix in
    let rec loop t i =
      Term.image t
        (I.pad ~b:1 traces.(i) <->
         I.strf "%d/%d" (i + 1) len <->
         I.string A.empty "Use left/right keys to navigate the stack trace" <->
         I.string A.empty "Press ESC to Quit");
      match Term.event t with
      | `Key (`Arrow `Left, _) -> loop t (Int.max 0 (i - 1))
      | `Key (`Arrow `Right, _) -> loop t (Int.min (len-1) (i + 1))
      | `Key (`Escape, _) -> ()
      | _ -> loop t i
    in
    let t = Term.create () in
    loop t (len-1);
    Term.release t
end
