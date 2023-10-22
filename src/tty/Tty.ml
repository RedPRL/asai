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

  module Style =
  struct
    let code (severity : Diagnostic.severity) : attr =
      match severity with
      | Hint -> A.fg A.blue
      | Info -> A.fg A.green
      | Warning -> A.fg A.yellow
      | Error -> A.fg A.red
      | Bug -> A.bg A.red ++ A.fg A.black

    let message (severity : Diagnostic.severity) (tag : Tag.t) : attr =
      match tag with
      | Extra _, _ -> A.empty
      | Main, _ -> code severity

    let highlight (severity : Diagnostic.severity) : Tag.t option -> attr =
      function
      | None -> A.empty
      | Some tag -> A.st A.underline ++ message severity tag

    let fringe = A.fg @@ A.gray 8

    let indentation = A.fg @@ A.gray 8
  end

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

  let render_code ~param =
    let attr = Style.code param.severity in
    hcat_with_pad ~pad:1
      [ I.string A.empty "￫"
      ; I.strf ~attr "%s[%s]:"
          (Diagnostic.string_of_severity param.severity)
          (Message.short_code param.message)
      ]

  (* [ ￭ examples/stlc/source.lambda] *)
  let render_source_header ~param:_ (s : Range.source) : I.t =
    match Range.title s with
    | None -> I.empty
    | Some title ->
      hcat_with_pad ~pad:1
        [ I.string A.empty "￭"
        ; I.string A.empty title
        ]

  let show_segment ~param (tag, seg) =
    I.string (Style.highlight param.severity tag) @@
    UserContent.replace_control ~tab_size:param.tab_size seg

  let render_line_tag ~param ((_, text) as tag) =
    let attr = Style.message param.severity tag in
    hcat_with_pad ~pad:1
      [ I.void param.line_number_width 0
      ; I.string A.empty "^"
      ; I.strf ~attr "%t" text
      ]

  let render_line_tags ~param tags =
    I.vcat @@ List.map (render_line_tag ~param) tags

  let show_line ~line_num ~param Explication.{segments; tags} =
    hcat_with_pad ~pad:1
      [ I.hsnap ~align:`Right param.line_number_width (I.string Style.fringe (Int.to_string line_num))
      ; I.string Style.fringe "|"
      ; I.hcat @@ List.map (show_segment ~param) segments
      ]
    <->
    render_line_tags ~param tags

  let render_block ~param Explication.{begin_line_num; end_line_num=_; lines} =
    I.vcat begin
      lines |> List.mapi @@ fun i line ->
      show_line ~line_num:(begin_line_num + i) ~param line
    end

  let render_part ~param Explication.{source; blocks} =
    render_source_header ~param source
    <->
    I.vcat begin
      blocks |> List.map @@ render_block ~param
    end

  let render_explication ~param parts =
    I.vcat begin
      parts |> List.map @@ fun p ->
      render_part ~param p
    end

  let render_unlocated_tag ~param ((_, text) as tag) =
    let attr = Style.message param.severity tag in
    hcat_with_pad ~pad:1
      [ I.string A.empty "￮"
      ; I.strf ~attr "%t" text
      ]

  let render_message ~param ?(end_padding=true) explication unlocated_tags =
    render_explication ~param explication
    <->
    I.vcat begin
      unlocated_tags |> List.map @@ render_unlocated_tag ~param
    end
    <->
    if end_padding then I.void 0 1 else I.empty

  let display_message ~param ?end_padding (explanation : Diagnostic.loctext) ~extra_remarks =
    let located_tags, unlocated_tags =
      let explanation = Tag.Main, explanation in
      let extra_remarks = List.mapi (fun i r -> Tag.Extra i, r) (Bwd.to_list extra_remarks) in
      List.partition_map
        (function
          | (tag, Range.{loc = None; value = text}) -> Either.Right (tag, text)
          | (tag, Range.{loc = Some sp; value = text}) -> Either.Left ((tag, text), sp))
        (explanation :: extra_remarks)
    in
    let explication =
      E.explicate ~block_splitting_threshold:param.block_splitting_threshold located_tags
    in
    let line_number_width = Int.max param.line_number_width (line_number_width explication) in
    render_message ~param:{param with line_number_width} ?end_padding explication unlocated_tags

  let display_backtrace ~param backtrace =
    let backtrace =
      I.vcat @@ Bwd.to_list @@
      Bwd.map (display_message ~param ~end_padding:false ~extra_remarks:Emp) backtrace
    in
    if I.height backtrace >= 1 then
      I.vcat
        [ I.string Style.indentation " ╭"
        ; I.tabulate 1
            (Int.max 0 (I.height backtrace - 2))
            (fun _ _ -> I.string Style.indentation " ┆")
        ; I.string Style.indentation " ╯"
        ]
      <|> backtrace
    else
      I.empty

  let display_diagnostic ~param ~explanation ~backtrace ~extra_remarks =
    render_code ~param
    <->
    SourceReader.run @@ fun () ->
    (if param.show_backtrace then display_backtrace ~param backtrace else I.empty)
    <->
    display_message ~param ~end_padding:true explanation ~extra_remarks

  let display ?(terminal_capacity) ?(output=Stdlib.stdout) ?(show_backtrace=true) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; message; explanation; backtrace; extra_remarks} =
    let param = {show_backtrace; line_breaking; block_splitting_threshold; tab_size; severity; message; line_number_width = 1} in
    Notty_unix.output_image ?cap:terminal_capacity ~fd:output @@ Notty_unix.eol @@ display_diagnostic ~param ~explanation ~backtrace ~extra_remarks

  let interact ?(input=Unix.stdin) ?(output=Unix.stdout) ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8)
      Diagnostic.{severity; message; explanation; extra_remarks; backtrace} =
    let param = {show_backtrace = true; line_breaking; block_splitting_threshold; tab_size; severity; message; line_number_width = 1} in
    let traces =
      SourceReader.run @@ fun () ->
      Bwd.snoc
        (Bwd.map (fun msg -> display_message ~param msg ~extra_remarks:Emp) backtrace)
        (display_message ~param explanation ~extra_remarks)
      |> Bwd.to_list |> Array.of_list
    in
    let len = Array.length traces in
    let images = traces |> Array.mapi @@ fun i image ->
      (render_code ~param <->
       image <->
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
