open Bwd
open Notty
open Notty.Infix

let string_of_severity : Diagnostic.severity -> string =
  function
  | Hint -> "hint"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Bug -> "bug"

let hcat_with_pad l =
  I.hcat @@ List.map (I.pad ~l:1) l

let vmap_cat f l = I.vcat @@ List.map f l

module E = Explicator.Make(TtyTag)

module Make (Message : Reporter.Message) =
struct
  (* parameters *)
  type param =
    {
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
    }

  (* calculating the width of line numbers *)

  let line_number_width explication : int =
    let max_line_number_block Explication.{end_line_num; _} = end_line_num in
    let max_line_number_part Explication.{blocks; _} =
      Utils.maximum @@ List.map max_line_number_block blocks
    in
    let max_line_number (parts : _ Explication.t) =
      Utils.maximum @@ List.map max_line_number_part parts
    in
    String.length @@ Int.to_string @@ max_line_number explication

  (* different parts of the display *)

  (* text *)

  let render_code ~param short_code =
    let attr = TtyStyle.code param.severity in
    hcat_with_pad
      [ I.string A.empty "￫"
      ; I.strf ~attr "%s[%s]"
          (string_of_severity param.severity)
          short_code
      ]

  (* [ ￭ examples/stlc/source.lambda] *)
  let render_source_header ~param:_ (s : Range.source) : I.t =
    match Range.title s with
    | None -> I.empty
    | Some title ->
      hcat_with_pad
        [ I.string A.empty "￭"
        ; I.string A.empty title
        ]

  let show_segment ~param (tag, seg) =
    I.string (TtyStyle.highlight param.severity tag) @@
    UserContent.replace_control ~tab_size:param.tab_size seg

  let render_line_tag ~param ((_, text) as tag) =
    let attr = TtyStyle.message param.severity tag in
    hcat_with_pad
      [ I.void param.line_number_width 0
      ; I.string A.empty "^"
      ; I.strf ~attr "%t" text
      ]

  let show_line ~line_num ~param Explication.{segments; tags} =
    hcat_with_pad
      [ I.hsnap ~align:`Right param.line_number_width (I.string TtyStyle.fringe (Int.to_string line_num))
      ; I.string TtyStyle.fringe "|"
      ; I.hcat @@ List.map (show_segment ~param) segments
      ]
    <->
    vmap_cat (render_line_tag ~param) tags

  let render_lines ~param ~begin_line_num lines =
    I.vcat begin
      lines |> List.mapi @@ fun i line ->
      show_line ~line_num:(begin_line_num + i) ~param line
    end

  let render_block ~param Explication.{begin_line_num; end_line_num=_; lines} =
    render_lines ~param ~begin_line_num lines

  let render_part ~param Explication.{source; blocks} =
    render_source_header ~param source
    <->
    vmap_cat (render_block ~param) blocks

  let render_explication ~param parts =
    vmap_cat (render_part ~param) parts

  let render_unlocated_tag ~param ((_, text) as tag) =
    let attr = TtyStyle.message param.severity tag in
    hcat_with_pad
      [ I.string A.empty "￮"
      ; I.strf ~attr "%t" text
      ]

  let display_message ~param (explanation : Diagnostic.loctext) ~extra_remarks =
    let located_tags, unlocated_tags =
      let explanation = TtyTag.Main, explanation in
      let extra_remarks = List.mapi (fun i r -> TtyTag.Extra i, r) (Bwd.to_list extra_remarks) in
      List.partition_map
        (function
          | (tag, Range.{loc = None; value = text}) -> Either.Right (tag, text)
          | (tag, Range.{loc = Some r; value = text}) -> Either.Left ((tag, text), r))
        (explanation :: extra_remarks)
    in
    let explication =
      E.explicate ~block_splitting_threshold:param.block_splitting_threshold located_tags
    in
    let line_number_width = Int.max param.line_number_width (line_number_width explication) in
    let param = {param with line_number_width} in
    render_explication ~param explication
    <->
    vmap_cat (render_unlocated_tag ~param) unlocated_tags

  let display_backtrace ~param backtrace =
    let backtrace =
      vmap_cat (display_message ~param ~extra_remarks:Emp) @@ Bwd.to_list backtrace
    in
    let indentation =
      I.pad ~l:1 @@
      match I.height backtrace with
      | 0 -> I.empty
      | 1 -> I.string TtyStyle.indentation "ꭍ"
      | h ->
        I.vcat
          [ I.string TtyStyle.indentation "╭"
          ; I.tabulate 1 (h - 2)
              (fun _ _ -> I.string TtyStyle.indentation "┆")
          ; I.string TtyStyle.indentation "╯"
          ]
    in
    indentation <|> backtrace

  let display_diagnostic ~param ~short_code ~explanation ~backtrace ~extra_remarks =
    render_code ~param short_code
    <->
    SourceReader.run @@ fun () ->
    display_backtrace ~param backtrace
    <->
    display_message ~param explanation ~extra_remarks
    <->
    I.void 0 1

  let display ?(terminal_capacity) ?(output=Stdlib.stdout) ?(show_backtrace=true) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; message; explanation; backtrace; extra_remarks} =
    let backtrace = if show_backtrace then backtrace else Emp in
    let param = {line_breaking; block_splitting_threshold; tab_size; severity; line_number_width = 1} in
    Notty_unix.output_image ?cap:terminal_capacity ~fd:output @@ Notty_unix.eol @@
    display_diagnostic ~param ~short_code:(Message.short_code message) ~explanation ~backtrace ~extra_remarks
end
