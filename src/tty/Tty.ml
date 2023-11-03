open Bwd

let string_of_severity : Diagnostic.severity -> string =
  function
  | Hint -> "hint"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Bug -> "bug"

module E = Explicator.Make(TtyTag)

(* calculating the width of line numbers *)

let[@tail_mod_cons] rec drop_last_empty =
  function
  | [] -> []
  | [""] -> []
  | x :: xs -> x :: (drop_last_empty[@tailcall]) xs

let highlight fmt = "@<0>%s" ^^ fmt ^^ "@<0>%s"

let indentf ~param fmt =
  Format.kasprintf @@ fun s ->
  let lines = drop_last_empty @@ String.split_on_char '\n' s in
  let p m line =
    Format.fprintf fmt (" " ^^ highlight "%s" ^^ "%s@.")
      (Ansi.style_string ~param TtyStyle.indentation)
      m
      (Ansi.reset_string ~param TtyStyle.indentation)
      line
  in
  match lines with
  | [] -> ()
  | [line] -> p "ꭍ" line
  | line :: lines -> p "╭" line;
    let rec go =
      function
      | [] -> assert false
      | [line] -> p "╯" line;
      | line :: lines -> p "┆" line;
        go lines
    in
    go lines

(* different parts of the display *)

let render_code ~param ~severity fmt short_code =
  let st = TtyStyle.code severity ~param in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "%s[%s]" ^^ "@.")
    "￫"
    (Ansi.style_string ~param st)
    (string_of_severity severity)
    short_code
    (Ansi.reset_string ~param st)

(* explication *)

module ExplicationRenderer :
sig
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
      ansi : Ansi.param;
    }

  val render : param:param -> Format.formatter -> TtyTag.t Explication.t -> unit
end
=
struct

  (* parameters *)
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
      ansi : Ansi.param;
    }

  (* [ ￭ examples/stlc/source.lambda] *)
  let render_source_header fmt (s : Range.source) =
    match Range.title s with
    | None -> ()
    | Some title -> Format.fprintf fmt " @<1>%s %s@." "￭" title

  let render_segment ~param fmt (tag, seg) =
    let st = TtyStyle.highlight ~param:param.ansi param.severity tag in
    Format.fprintf fmt (highlight "%s")
      (Ansi.style_string ~param:param.ansi st)
      (UserContent.replace_control ~tab_size:param.tab_size seg)
      (Ansi.reset_string ~param:param.ansi st)

  let render_line_tag ~param fmt ((_, text) as tag) =
    let st = TtyStyle.message ~param:param.ansi param.severity tag in
    Format.fprintf fmt (" %*s ^ " ^^ highlight "@[%t@]" ^^ "@.")
      param.line_number_width ""
      (Ansi.style_string ~param:param.ansi st)
      text
      (Ansi.reset_string ~param:param.ansi st)

  let render_line ~line_num ~param fmt Explication.{segments; tags} =
    Format.fprintf fmt (" " ^^ highlight "%*d |" ^^ " ")
      (Ansi.style_string ~param:param.ansi TtyStyle.fringe)
      param.line_number_width line_num
      (Ansi.reset_string ~param:param.ansi TtyStyle.fringe);
    List.iter (render_segment ~param fmt) segments;
    Format.fprintf fmt "@.";
    List.iter (render_line_tag ~param fmt) tags

  let render_lines ~param ~begin_line_num fmt lines =
    lines |> List.iteri @@ fun i line ->
    render_line ~line_num:(begin_line_num + i) ~param fmt line

  let render_block ~param fmt Explication.{begin_line_num; end_line_num=_; lines} =
    render_lines ~param ~begin_line_num fmt lines

  let render_part ~param fmt Explication.{source; blocks} =
    render_source_header fmt source;
    List.iter (render_block ~param fmt) blocks

  let render ~param fmt parts =
    List.iter (render_part ~param fmt) parts
end

let render_unlocated_tag ~severity ~ansi fmt ((_, text) as tag) =
  let st = TtyStyle.message ~param:ansi severity tag in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "@[%t@]" ^^ "@.")
    "￮"
    (Ansi.style_string ~param:ansi st)
    text
    (Ansi.reset_string ~param:ansi st)

module TopLevelRenderer :
sig
  type param =
    {
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      ansi : Ansi.param;
    }

  val render_diagnostic : param:param -> Format.formatter -> string Diagnostic.t -> unit
end
=
struct
  type param =
    {
      line_breaking : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      ansi : Ansi.param;
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

  let render_textloc ~param ~severity ~extra_remarks fmt (textloc : Diagnostic.loctext) =
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
    let param = {ExplicationRenderer.severity = severity; tab_size = param.tab_size; line_number_width; ansi = param.ansi} in
    ExplicationRenderer.render ~param fmt explication;
    List.iter (render_unlocated_tag ~severity:param.severity ~ansi:param.ansi fmt) unlocated_tags

  let render_backtrace ~param ~severity fmt backtrace =
    indentf ~param:param.ansi fmt "%a"
      (fun fmt () -> List.iter (render_textloc ~param ~severity ~extra_remarks:Emp fmt) @@ Bwd.to_list backtrace) ()

  let render_diagnostic ~param fmt Diagnostic.{severity; message = short_code; explanation; backtrace; extra_remarks} =
    render_code ~severity ~param:param.ansi fmt short_code;
    render_backtrace ~param ~severity fmt backtrace;
    render_textloc ~param ~severity ~extra_remarks fmt explanation;
end

module Make (Message : MinimumSigs.Message) =
struct
  let display ?(output=Stdlib.stdout) ?use_ansi ?use_color ?(show_backtrace=true)
      ?(line_breaking=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8) d =
    let d = if show_backtrace then d else {d with Diagnostic.backtrace = Emp} in
    let d = Diagnostic.map Message.short_code d in
    let ansi = Ansi.Test.guess ?use_ansi ?use_color output in
    let param = {TopLevelRenderer.line_breaking; block_splitting_threshold; tab_size; ansi} in
    let fmt = Format.formatter_of_out_channel output in
    SourceReader.run @@ fun () ->
    TopLevelRenderer.render_diagnostic ~param fmt d;
    Format.pp_print_newline fmt ()
end
