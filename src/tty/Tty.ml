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

let map_vcat f l = I.vcat @@ List.map f l

module E = Explicator.Make(TtyTag)

(* calculating the width of line numbers *)

let render_indentation height =
  I.pad ~l:1 @@
  match height with
  | 0 -> I.empty
  | h ->
    I.vcat
      [ I.tabulate 1 (h - 1)
          (fun _ _ -> I.string TtyStyle.indentation "┆")
      ; I.string TtyStyle.indentation "╯"
      ]

(* different parts of the display *)

let render_code ~severity short_code =
  let attr = TtyStyle.code severity in
  hcat_with_pad
    [ I.string A.empty "￫"
    ; I.strf ~attr "%s[%s]"
        (string_of_severity severity)
        short_code
    ]

(* explication *)

module ExplicationRenderer :
sig
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
    }

  val render : param:param -> TtyTag.t Explication.t -> I.t
end
=
struct

  (* parameters *)
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
    }

  (* [ ￭ examples/stlc/source.lambda] *)
  let render_source_header ~param:_ (s : Range.source) : I.t =
    match Range.title s with
    | None -> I.empty
    | Some title ->
      hcat_with_pad
        [ I.string A.empty "￭"
        ; I.string A.empty title
        ]

  let render_segment ~param (tag, seg) =
    I.string (TtyStyle.highlight param.severity tag) @@
    UserContent.replace_control ~tab_size:param.tab_size seg

  let render_line_tag ~param ((_, text) as tag) =
    let attr = TtyStyle.message param.severity tag in
    hcat_with_pad
      [ I.void param.line_number_width 0
      ; I.string A.empty "^"
      ; I.strf ~attr "%t" text
      ]

  let render_line ~line_num ~param Explication.{segments; tags} =
    hcat_with_pad
      [ I.hsnap ~align:`Right param.line_number_width (I.string TtyStyle.fringe (Int.to_string line_num))
      ; I.string TtyStyle.fringe "|"
      ; I.hcat @@ List.map (render_segment ~param) segments
      ]
    <->
    map_vcat (render_line_tag ~param) tags

  let render_lines ~param ~begin_line_num lines =
    I.vcat begin
      lines |> List.mapi @@ fun i line ->
      render_line ~line_num:(begin_line_num + i) ~param line
    end

  let render_block ~param Explication.{begin_line_num; end_line_num=_; lines} =
    render_lines ~param ~begin_line_num lines

  let render_part ~param Explication.{source; blocks} =
    render_source_header ~param source
    <->
    map_vcat (render_block ~param) blocks

  let render ~param parts =
    map_vcat (render_part ~param) parts
end

let render_unlocated_tag ~severity ((_, text) as tag) =
  let attr = TtyStyle.message severity tag in
  hcat_with_pad
    [ I.string A.empty "￮"
    ; I.strf ~attr "%t" text
    ]

module TopLevelRenderer :
sig
  type param =
    {
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
    }

  val render_diagnostic : param:param -> string Diagnostic.t -> I.t
end
=
struct
  type param =
    {
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
    }

  let line_number_width explication : int =
    let max_line_number_block Explication.{end_line_num; _} = end_line_num in
    let max_line_number_part Explication.{blocks; _} =
      Utils.maximum @@ List.map max_line_number_block blocks
    in
    let max_line_number (parts : _ Explication.t) =
      Utils.maximum @@ List.map max_line_number_part parts
    in
    String.length @@ Int.to_string @@ max_line_number explication

  let render_textloc ~param ~severity (textloc : Diagnostic.loctext) ~extra_remarks =
    let located_tags, unlocated_tags =
      let main = TtyTag.Main, textloc in
      let extra_remarks = List.mapi (fun i r -> TtyTag.Extra i, r) (Bwd.to_list extra_remarks) in
      List.partition_map
        (function
          | (tag, Range.{loc = None; value = text}) -> Either.Right (tag, text)
          | (tag, Range.{loc = Some r; value = text}) -> Either.Left ((tag, text), r))
        (main :: extra_remarks)
    in
    let explication =
      E.explicate ~block_splitting_threshold:param.block_splitting_threshold located_tags
    in
    let line_number_width = line_number_width explication in
    let param = {ExplicationRenderer.severity = severity; tab_size = param.tab_size; line_number_width} in
    ExplicationRenderer.render ~param explication
    <->
    map_vcat (render_unlocated_tag ~severity:param.severity) unlocated_tags

  let render_backtrace ~param ~severity backtrace =
    let backtrace =
      map_vcat (render_textloc ~param ~severity ~extra_remarks:Emp) @@ Bwd.to_list backtrace
    in
    render_indentation (I.height backtrace)
    <|>
    backtrace

  let render_diagnostic ~param Diagnostic.{severity; message = short_code; explanation; backtrace; extra_remarks} =
    render_code ~severity short_code
    <->
    render_backtrace ~param ~severity backtrace
    <->
    render_textloc ~param ~severity explanation ~extra_remarks
    <->
    I.void 0 1
end

module Make (Message : MinimumSigs.Message) =
struct
  let display ?(terminal_capacity) ?(output=Stdlib.stdout) ?(show_backtrace=true) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8) d =
    let d = if show_backtrace then d else {d with Diagnostic.backtrace = Emp} in
    let param = {TopLevelRenderer.line_breaking; block_splitting_threshold; tab_size} in
    Notty_unix.output_image ?cap:terminal_capacity ~fd:output @@ Notty_unix.eol @@
    SourceReader.run @@ fun () ->
    TopLevelRenderer.render_diagnostic ~param (Diagnostic.map Message.short_code d)
end
