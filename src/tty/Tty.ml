open Bwd
open Notty
open Notty.Infix

module Tag = struct
  type index = Main | Extra of int
  type t = index * Diagnostic.text
  let equal (x : t) y = fst x = fst y
  let priority =
    function
    | Main, _ -> -1
    | Extra i, _ -> i
  let dump fmt =
    function
    | Main, _ -> Format.pp_print_string fmt "Main"
    | Extra i, _ -> Format.fprintf fmt "Extra %d" i
end

module E = Explicator.Make(Tag)

module Make (Message : Reporter.Message) =
struct
  (* helper functions *)

  let hcat_with_pad ~pad l =
    I.hcat @@ List.map (I.pad ~l:pad) l

  (* styles *)

  let message_style (severity : Diagnostic.severity) (tag : Tag.t) : attr =
    match tag with
    | Extra _, _ -> A.empty
    | Main, _ ->
      match severity with
      | Hint -> A.fg A.blue
      | Info -> A.fg A.green
      | Warning -> A.fg A.yellow
      | Error -> A.fg A.red
      | Bug -> A.bg A.red ++ A.fg A.black

  let highlight_style (severity : Diagnostic.severity) : Tag.t option -> attr =
    function
    | None -> A.empty
    | Some tag -> A.st A.underline ++ message_style severity tag

  let fringe_style = A.fg @@ A.gray 8

  (* parameters *)
  type param =
    {
      show_backtrace : bool;
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      severity : Diagnostic.severity;
      message : Message.t;
      line_number_width : int;
    }

  (* text *)

  let render_tag ~param ~show_code ((index, text) as tag) =
    let attr = message_style param.severity tag in
    I.pad ~l:1 begin
      (if show_code && index = Tag.Main
       then
         I.strf ~attr "%s[%s]:"
           (Diagnostic.string_of_severity param.severity)
           (Message.short_code param.message)
       else I.empty)
      <->
      I.strf ~attr "%t" text
    end

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

  (* [ ╒══ examples/stlc/source.lambda] *)
  (* [ │ ] *)
  let render_source_header ~param : Span.source -> I.t =
    function
    | `String {title=None; _} ->
      hcat_with_pad ~pad:1
        [ I.void param.line_number_width 0
        ; I.string fringe_style "┯"
        ]
    | `File title | `String {title=Some title; _} ->
      hcat_with_pad ~pad:1
        [ I.void param.line_number_width 0
        ; I.string fringe_style "╒══" <-> I.string fringe_style "│"
        ; I.string A.empty title
        ]

  let show_code_segment ~param (tag, seg) =
    I.string (highlight_style param.severity tag) (UserContent.replace_control ~tab_size:param.tab_size seg)

  (* [ ┊ ] *)
  let render_block_sep ~param =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "┊"
      ]

  let render_line_tags ~param ~show_code tags =
    I.vcat begin
      tags |> List.mapi @@ fun i b ->
      (if i = 0 then render_block_sep ~param else I.empty)
      <->
      render_tag ~param ~show_code b
      <->
      render_block_sep ~param
    end

  let show_line ~line_num ~param ~show_code Explication.{segments; tags} =
    hcat_with_pad ~pad:1
      [ I.hsnap ~align:`Right param.line_number_width (I.string fringe_style (Int.to_string line_num))
      ; I.string fringe_style "│"
      ; I.hcat @@ List.map (show_code_segment ~param) segments
      ]
    <->
    render_line_tags ~param ~show_code tags

  (* [3 ⇃ no, it is not my fault!!!] *)
  let render_block ~param ~show_code Explication.{begin_line_num; end_line_num=_; lines} =
    I.vcat @@ List.mapi (fun i line -> show_line ~line_num:(begin_line_num + i) ~param ~show_code line) lines

  (* [ ┷ ] *)
  let render_code_part_end ~param =
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string fringe_style "┷"
      ]

  let render_part ~param ~show_code Explication.{source; blocks} =
    render_source_header ~param source
    <->
    I.vcat begin
      blocks |> List.mapi @@ fun i b ->
      (if i > 0 then render_block_sep ~param else I.empty)
      <->
      render_block ~param ~show_code b
    end
    <->
    render_code_part_end ~param

  let render_explication ~param ~show_code parts =
    I.vcat @@ List.map (render_part ~param ~show_code) parts

  let render_message ~param ~show_code explication tags =
    render_explication ~param ~show_code explication
    <->
    I.vcat (List.map (fun t -> render_tag ~param ~show_code t) tags)

  let display_message ~param ~show_code (explanation : Diagnostic.loctext) ~extra_remarks =
    let located_tags, unlocated_tags =
      let explanation = Tag.Main, explanation in
      let extra_remarks = List.mapi (fun i r -> Tag.Extra i, r) (Bwd.to_list extra_remarks) in
      List.partition_map
        (function
          | (tag, Span.{loc = None; value = text}) -> Either.Right (tag, text)
          | (tag, Span.{loc = Some sp; value = text}) -> Either.Left ((tag, text), sp))
        (explanation :: extra_remarks)
    in
    let explication =
      E.explicate ~block_splitting_threshold:param.block_splitting_threshold located_tags
    in
    let line_number_width = Int.max param.line_number_width (line_number_width explication) in
    render_message ~param:{param with line_number_width} ~show_code explication unlocated_tags

  let display_backtrace ~param backtrace =
    let indentation_style = A.fg @@ A.gray 8 in
    let backtrace =
      Bwd.to_list @@ Bwd.map (display_message ~param ~show_code:false ~extra_remarks:Emp) backtrace
    in
    let backtrace =
      I.vcat @@
      List.mapi
        (fun i image -> (if i > 0 then I.void 0 1 else I.empty) <-> image)
        backtrace
    in
    I.vcat
      [ I.string indentation_style " ╭"
      ; I.tabulate 1 (I.height backtrace) (fun _ _ -> I.string indentation_style " ┆") <|> backtrace
      ; I.string indentation_style " ╯"
      ]

  let display_diagnostic ~param ~explanation ~backtrace ~extra_remarks =
    SourceReader.run @@ fun () ->
    (if param.show_backtrace && backtrace <> Emp then display_backtrace ~param backtrace else I.empty)
    <->
    display_message ~param ~show_code:true explanation ~extra_remarks
    <->
    I.void 0 1 (* new line *)

  let display ?(terminal_capacity) ?(output=Stdlib.stdout) ?(show_backtrace=true) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; message; explanation; backtrace; extra_remarks} =
    let param = {show_backtrace; line_breaking; block_splitting_threshold; tab_size; severity; message; line_number_width = 2} in
    Notty_unix.output_image ?cap:terminal_capacity ~fd:output @@ Notty_unix.eol @@ display_diagnostic ~param ~explanation ~backtrace ~extra_remarks

  let interactive_trace ?(input=Unix.stdin) ?(output=Unix.stdout) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; message; explanation; extra_remarks; backtrace} =
    let param = {show_backtrace = true; line_breaking; block_splitting_threshold; tab_size; severity; message; line_number_width = 2} in
    let traces =
      SourceReader.run @@ fun () ->
      Bwd.snoc
        (backtrace |> Bwd.map (fun msg -> display_message ~param ~show_code:true msg ~extra_remarks:Emp))
        (display_message ~param ~show_code:true explanation ~extra_remarks)
      |> Bwd.to_list |> Array.of_list
    in
    let len = Array.length traces in
    let images = traces |> Array.mapi @@ fun i image ->
      (image <->
       I.void 0 1 <->
       I.strf "%d/%d" (i + 1) len <->
       I.string A.empty "Use left/right keys to navigate the stack trace" <->
       I.string A.empty "Press ESC to Quit")
    in
    let rec loop t i =
      Notty_unix.Term.image t images.(i);
      match Notty_unix.Term.event t with
      | `Key (`Arrow `Left, _) -> loop t (Int.max 0 (i - 1))
      | `Key (`Arrow `Right, _) -> loop t (Int.min (len-1) (i + 1))
      | `Key (`Escape, _) -> ()
      | _ -> loop t i
    in
    let t = Notty_unix.Term.create ~input ~output () in
    loop t (len-1);
    Notty_unix.Term.release t
end
