open Bwd

let string_of_severity : Diagnostic.severity -> string =
  function
  | Hint -> "hint"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Bug -> "bug"

module SM = SourceMarker.Make(TtyTag)

(* calculating the width of line numbers *)

let[@tail_mod_cons] rec drop_last_empty =
  function
  | [] -> []
  | [""] -> []
  | x :: xs -> x :: (drop_last_empty[@tailcall]) xs

let indent_decorations len i =
  match len, i with
  | 1, _ -> "ꭍ"
  | _, 0 -> "╭"
  | n, i when i = n-1 -> "╯"
  | _ -> "┆"

let highlight fmt = "@<0>%s" ^^ fmt ^^ "@<0>%s"

let indentf ~param fmt =
  Format.kasprintf @@ fun s ->
  let lines = drop_last_empty @@ String.split_on_char '\n' s in
  let num_lines = List.length lines in
  let p m line =
    Format.fprintf fmt (" " ^^ highlight "@<1>%s" ^^ "%s@.")
      (Ansi.style_string ~param TtyStyle.indentation)
      m
      (Ansi.reset_string ~param TtyStyle.indentation)
      line
  in
  List.iteri (fun i line -> p (indent_decorations num_lines i) line) lines

(* different parts of the display *)

let render_code ~param ~severity fmt short_code =
  let style = TtyStyle.code severity ~param in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "%s[%s]" ^^ "@.")
    "￫"
    (Ansi.style_string ~param style)
    (string_of_severity severity)
    short_code
    (Ansi.reset_string ~param style)

(* marked source *)

module MarkedSourceRenderer :
sig
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
      ansi : Ansi.param;
    }

  val render : param:param -> Format.formatter -> TtyTag.t MarkedSource.t -> unit
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

  let render_line_marker ~param fmt ((_, text) as tag) =
    let style = TtyStyle.message ~param:param.ansi param.severity tag in
    Format.fprintf fmt (" %*s " ^^ highlight "^" ^^ " " ^^ highlight "@[%t@]" ^^ "@.")
      param.line_number_width ""
      (Ansi.style_string ~param:param.ansi TtyStyle.fringe)
      (Ansi.reset_string ~param:param.ansi TtyStyle.fringe)
      (Ansi.style_string ~param:param.ansi style)
      text
      (Ansi.reset_string ~param:param.ansi style)

  let render_styled_segment ~param fmt tag segment =
    let style = TtyStyle.highlight ~param:param.ansi param.severity tag in
    Format.fprintf fmt (highlight "%s")
      (Ansi.style_string ~param:param.ansi style)
      (StringUtils.replace_control ~tab_size:param.tab_size segment)
      (Ansi.reset_string ~param:param.ansi style)

  (* Current design:

     ‹let x◂POS₀▸ = 1› in let ‹x› = «1 + ‹x›»◂POS₁▸
     ‹let x◂POS₀▸ = 1›₀ in let ‹x›₁ = «1 + ‹x›₂»◂POS₁▸
  *)

  let render_line ~line_num ~param fmt init_tag_set MarkedSource.{tokens; markers} =
    let go set =
      function
      | MarkedSource.String s ->
        render_styled_segment ~param fmt (TtyTagSet.prioritized set) s; set
      | MarkedSource.Marker RangeEnd t ->
        TtyTagSet.remove t set
      | MarkedSource.Marker Point t ->
        render_styled_segment ~param fmt (Some t) "‹POS›"; set
      | MarkedSource.Marker RangeBegin t ->
        TtyTagSet.add t set
    in
    Format.fprintf fmt (" " ^^ highlight "%*d |" ^^ " ")
      (Ansi.style_string ~param:param.ansi TtyStyle.fringe)
      param.line_number_width line_num
      (Ansi.reset_string ~param:param.ansi TtyStyle.fringe);
    let end_tag_set = List.fold_left go init_tag_set tokens in
    Format.fprintf fmt "@.";
    List.iter (render_line_marker ~param fmt) markers;
    end_tag_set

  let render_lines ~param ~begin_line_num fmt lines =
    ignore @@ List.fold_left
      (fun (line_num, set) line ->
         let set = render_line ~line_num ~param fmt set line in
         (line_num+1, set))
      (begin_line_num, TtyTagSet.empty)
      lines

  let render_block ~param fmt MarkedSource.{begin_line_num; end_line_num=_; lines} =
    render_lines ~param ~begin_line_num fmt lines

  let render_part ~param fmt MarkedSource.{source; blocks} =
    render_source_header fmt source;
    List.iter (render_block ~param fmt) blocks

  let render ~param fmt parts =
    List.iter (render_part ~param fmt) parts
end

let render_unlocated_tag ~severity ~ansi fmt ((_, text) as tag) =
  let style = TtyStyle.message ~param:ansi severity tag in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "@[%t@]" ^^ "@.")
    "￮"
    (Ansi.style_string ~param:ansi style)
    text
    (Ansi.reset_string ~param:ansi style)

module DiagnosticRenderer :
sig
  type param =
    {
      debug : bool;
      line_breaks : [`Unicode | `Traditional];
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
      debug : bool;
      line_breaks : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      ansi : Ansi.param;
    }

  let line_number_width marked_source : int =
    let max_line_number_block MarkedSource.{end_line_num; _} = end_line_num in
    let max_line_number_part MarkedSource.{blocks; _} =
      Utils.maximum @@ List.map max_line_number_block blocks
    in
    let max_line_number (parts : _ MarkedSource.t) =
      Utils.maximum @@ List.map max_line_number_part parts
    in
    String.length @@ Int.to_string @@ max_line_number marked_source

  let render_textloc ~param ~severity ~extra_remarks fmt (textloc : Loctext.t) =
    let located_tags, unlocated_tags =
      let main = TtyTag.Main, textloc in
      let extra_remarks = List.mapi (fun i r -> TtyTag.Extra i, r) (Bwd.to_list extra_remarks) in
      List.partition_map
        (function
          | (tag, Range.{loc = None; value = text}) -> Either.Right (tag, text)
          | (tag, Range.{loc = Some r; value = text}) -> Either.Left (r, (tag, text)))
        (main :: extra_remarks)
    in
    let marked_source =
      SM.mark ~block_splitting_threshold:param.block_splitting_threshold ~debug:param.debug located_tags
    in
    let line_number_width = line_number_width marked_source in
    let param = {MarkedSourceRenderer.severity = severity; tab_size = param.tab_size; line_number_width; ansi = param.ansi} in
    MarkedSourceRenderer.render ~param fmt marked_source;
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
      ?(line_breaks=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8) ?(debug=false) d =
    let d = if show_backtrace then d else {d with Diagnostic.backtrace = Emp} in
    let d = Diagnostic.map Message.short_code d in
    let ansi = Ansi.Test.guess ?use_ansi ?use_color output in
    let param = {DiagnosticRenderer.debug; line_breaks; block_splitting_threshold; tab_size; ansi} in
    let fmt = Format.formatter_of_out_channel output in
    SourceReader.run @@ fun () ->
    DiagnosticRenderer.render_diagnostic ~param fmt d;
    Format.pp_print_newline fmt ()
end
