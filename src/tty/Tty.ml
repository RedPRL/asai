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
    🭁 examples/stlc/source.lambda
    ⇃
  3 ⇃ no, it is not my fault!!!
  4 ⇃ I was just calling some function!!!
    ⇃
 [E002] When looking into the meaning of 42

    🭁 examples/stlc/example.lambda
    │
  1 │ (check (λ ä (λ 123
  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
    ┊
 20 │ ahhhhhhhhhhhhhhhhhh
 21 │ noooooooooooooooooo
    ┷
 [E002] Why am I checking the term (→ ℕ (→ ℕ ℕ))
        which looks amazing!!!

    🭁 examples/stlc/example2.lambda
    │
  3 │ let x = 1
  4 │ let y = 1
    ┷
    🭁 examples/stlc/example3.lambda
    │
  8 │ assert (asai is cool)
    ┷
*)

  (* helper functions *)

  let hcat_with_pad ~pad l =
    I.hcat @@ List.map (I.pad ~l:pad) l

  let vcat_with_pad ~pad l =
    I.vcat @@ List.map (I.pad ~b:pad) l

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

  (* [ 🭁 examples/stlc/source.lambda] *)
  let render_file_header ~line_number_width ~is_backtrace:_ file_path =
    hcat_with_pad ~pad:1
      [ I.void line_number_width 0
      ; I.string fringe_style "🭁"
      ; I.string A.empty file_path
      ]

  (* [ ⇃ ] or [ │ ] *)
  let render_file_header_padding ~line_number_width ~is_backtrace =
    hcat_with_pad ~pad:1
      [ I.void line_number_width 0
      ; I.string fringe_style (if is_backtrace then "⇃" else "│")
      ]

  let show_code_segment ~tab_size severity Explication.{style; value = seg} =
    I.string (highlight_style severity style) (UserContent.replace_control ~tab_size seg)

  let show_code_line ~tab_size severity (segs : _ Explication.line) =
    I.hcat @@ List.map (show_code_segment ~tab_size severity) segs

  (* [3 ⇃ no, it is not my fault!!!] *)
  let render_block ~tab_size ~line_number_width~is_backtrace ~severity Explication.{start_line_num; lines} =
    List.mapi (fun i line ->
        let line_num = start_line_num + i in
        hcat_with_pad ~pad:1
          [ I.hsnap ~align:`Right line_number_width (I.string fringe_style (Int.to_string line_num))
          ; I.string fringe_style (if is_backtrace then "⇃" else "│")
          ; show_code_line ~tab_size severity line
          ]
      ) lines

  (* [ ┊ ] *)
  let render_code_block_sep ~line_number_width ~is_backtrace:_ =
    hcat_with_pad ~pad:1
      [ I.void line_number_width 0
      ; I.string fringe_style "┊"
      ]

  (* [ ⇃ ] or [ ┷ ] *)
  let render_code_part_end ~line_number_width ~is_backtrace =
    hcat_with_pad ~pad:1
      [ I.void line_number_width 0
      ; I.string fringe_style (if is_backtrace then "⇃" else "┷")
      ]

  let render_part ~tab_size ~line_number_width ~is_backtrace ~severity Explication.{file_path; blocks} =
    [ render_file_header ~line_number_width ~is_backtrace file_path
    ; render_file_header_padding ~line_number_width ~is_backtrace
    ] @
    begin
      List.concat @@
      List.mapi
        (fun i b ->
           List.concat
             [ if i = 0 then [] else [ render_code_block_sep ~line_number_width ~is_backtrace ]
             ; render_block ~tab_size ~line_number_width ~is_backtrace ~severity b
             ]
        ) blocks
    end @
    [ render_code_part_end ~line_number_width ~is_backtrace ]

  let render_explication ~tab_size ~line_number_width ~is_backtrace ~severity parts =
    I.vcat @@ List.concat_map (render_part ~tab_size ~line_number_width ~is_backtrace ~severity) parts

  (* message *)
  let render_text ~line_number_width:_ ~is_backtrace:_ ~code text =
    hcat_with_pad ~pad:1
      [ I.strf "[%s]" (Code.to_string code)
      ; I.strf "%t" text
      ]

  let render_message ~tab_size ~line_number_width ~is_backtrace ~severity ~code explication text =
    render_explication ~tab_size ~line_number_width ~is_backtrace ~severity explication
    <->
    render_text ~line_number_width ~is_backtrace ~code text

  let display_message ~is_backtrace ~line_breaking ~severity ~code (msg : Diagnostic.message) ~additional_messages =
    let explication =
      let style s x = Explication.{value = x; style = s} in
      let main_span = Option.to_list @@ Option.map (style Style.HighlightedPrimary) msg.loc in
      let additional_spans = List.filter_map (fun x -> Option.map (style Style.Additional) x.Span.loc) additional_messages in
      E.explicate ~line_breaking ~splitting_threshold:5 (main_span @ additional_spans)
    in
    let line_number_width = line_number_width explication in
    render_message ~line_number_width ~is_backtrace ~severity ~code explication msg.value

  let display_diagnostic ~line_breaking ~tab_size show_backtrace Diagnostic.{severity; code; message; backtrace; additional_messages} =
    FileReader.run @@ fun () ->
    let msgs =
      Bwd.snoc
        (if show_backtrace then
           Bwd.map (display_message ~is_backtrace:true ~line_breaking ~tab_size ~severity ~code ~additional_messages:[]) backtrace
         else
           Emp)
        (display_message ~is_backtrace:false ~line_breaking ~tab_size ~severity ~code message ~additional_messages)
    in
    vcat_with_pad ~pad:1 (Bwd.to_list msgs)

  module F = Explicator.Make(FileReader)

  let display ?(line_breaking=`Traditional) ?(tab_size=8) ?(show_backtrace = false) diag =
    Notty_unix.output_image (display_diagnostic ~line_breaking ~tab_size show_backtrace diag)

  let interactive_trace ?(line_breaking=`Traditional) ?(tab_size=8) Diagnostic.{code; severity; message; additional_messages; backtrace} =
    let traces =
      FileReader.run @@ fun () ->
      Bwd.snoc
        (backtrace |> Bwd.map (fun msg -> display_message ~line_breaking ~tab_size ~is_backtrace:true ~severity ~code msg ~additional_messages:[]))
        (display_message ~line_breaking ~tab_size ~is_backtrace:false ~severity ~code message ~additional_messages)
      |> Bwd.to_list |> Array.of_list
    in
    let len = Array.length traces in
    let open Notty_unix in
    let rec loop t i =
      Term.image t (I.pad ~b:1 traces.(i) <->
                    I.strf "%d/%d" (i + 1) len <->
                    I.string A.empty "Use arrow keys to navigate the stack trace" <->
                    I.string A.empty "Press ESC to Quit");
      match Term.event t with
      | `Key (`Arrow `Up, _) -> loop t (Int.max 0 (i - 1))
      | `Key (`Arrow `Down, _) -> loop t (Int.min (len-1) (i + 1))
      | `Key (`Escape, _) -> ()
      | _ -> loop t i
    in
    let t = Term.create () in
    loop t (len-1);
    Term.release t
end
