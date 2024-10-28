open Bwd

let string_of_severity : Diagnostic.severity -> string =
  function
  | Hint -> "hint"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Bug -> "bug"

type marker =
  ansi:[ `Enabled_with_color | `Enabled_without_color | `Disabled ]
  -> [ `Main_message | `Extra_remark of int ]
  -> [ `Range_begin
     | `Range_end of [`End_of_line | `End_of_file] option
     | `Point of [`End_of_line | `End_of_file] option
     ]
  -> string

let default_marker ~ansi:_ _ =
  function
  | `Range_begin | `Range_end _ -> ""
  | `Point Some `End_of_line -> "‹EOL›"
  | `Point Some `End_of_file -> "‹EOF›"
  | `Point None -> "‹POS›"

module SM = Source_marker.Make(Tty_tag)

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
      (Ansi.style_string ~param Tty_style.indentation)
      m
      (Ansi.reset_string ~param Tty_style.indentation)
      line
  in
  List.iteri (fun i line -> p (indent_decorations num_lines i) line) lines

(* different parts of the display *)

let render_code ~param ~severity fmt short_code =
  let style = Tty_style.code severity ~param in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "%s[%s]" ^^ "@.")
    "￫"
    (Ansi.style_string ~param style)
    (string_of_severity severity)
    short_code
    (Ansi.reset_string ~param style)

(* marked source *)

module Marked_source_renderer :
sig
  type param =
    {
      tab_size : int;
      severity : Diagnostic.severity;
      line_number_width : int;
      ansi : Ansi.param;
      marker : marker;
    }

  val render : param:param -> Format.formatter -> Tty_tag.t Marked_source.t -> unit
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
      marker : marker;
    }

  (* [ ￭ examples/stlc/source.lambda] *)
  let render_source_header fmt (s : Range.source) =
    match Range.title s with
    | None -> ()
    | Some title -> Format.fprintf fmt " @<1>%s %s@." "￭" title

  let render_line_mark ~param fmt ((_, text) as tag) =
    let style = Tty_style.message ~param:param.ansi param.severity tag in
    Format.fprintf fmt (" %*s " ^^ highlight "^" ^^ " " ^^ highlight "@[%t@]" ^^ "@.")
      param.line_number_width ""
      (Ansi.style_string ~param:param.ansi Tty_style.fringe)
      (Ansi.reset_string ~param:param.ansi Tty_style.fringe)
      (Ansi.style_string ~param:param.ansi style)
      text
      (Ansi.reset_string ~param:param.ansi style)

  let render_styled_segment ~param fmt tag segment =
    if segment <> "" then
      let style = Tty_style.highlight ~param:param.ansi param.severity tag in
      Format.fprintf fmt (highlight "%s")
        (Ansi.style_string ~param:param.ansi style)
        (String_utils.replace_control ~tab_size:param.tab_size segment)
        (Ansi.reset_string ~param:param.ansi style)

  let render_line ~line_num ~param fmt init_tag_set Marked_source.{tokens; marks} =
    let go set =
      function
      | Marked_source.String s ->
        render_styled_segment ~param fmt (Tty_tag_set.prioritized set) s; set
      | Marked_source.Mark (sp, m) ->
        let (idx, _ as tag), m, next_set =
          match m with
          | Range_end t -> t, `Range_end sp, Tty_tag_set.remove t set
          | Range_begin t -> t, `Range_begin, Tty_tag_set.add t set
          | Point t -> t, `Point sp, set
        in
        let mark = param.marker ~ansi:param.ansi idx m in
        render_styled_segment ~param fmt (Some tag) mark; next_set
    in
    Format.fprintf fmt (" " ^^ highlight "%*d |" ^^ " ")
      (Ansi.style_string ~param:param.ansi Tty_style.fringe)
      param.line_number_width line_num
      (Ansi.reset_string ~param:param.ansi Tty_style.fringe);
    let end_tag_set = List.fold_left go init_tag_set tokens in
    Format.fprintf fmt "@.";
    List.iter (render_line_mark ~param fmt) marks;
    end_tag_set

  let render_lines ~param ~begin_line_num fmt lines =
    ignore @@ List.fold_left
      (fun (line_num, set) line ->
         let set = render_line ~line_num ~param fmt set line in
         (line_num+1, set))
      (begin_line_num, Tty_tag_set.empty)
      lines

  let render_block ~param fmt Marked_source.{begin_line_num; end_line_num=_; lines} =
    render_lines ~param ~begin_line_num fmt lines

  let render_part ~param fmt Marked_source.{source; blocks} =
    render_source_header fmt source;
    List.iter (render_block ~param fmt) blocks

  let render ~param fmt parts =
    List.iter (render_part ~param fmt) parts
end

let render_unlocated_tag ~severity ~ansi fmt ((_, text) as tag) =
  let style = Tty_style.message ~param:ansi severity tag in
  Format.fprintf fmt (" @<1>%s " ^^ highlight "@[%t@]" ^^ "@.")
    "￮"
    (Ansi.style_string ~param:ansi style)
    text
    (Ansi.reset_string ~param:ansi style)

module Diagnostic_renderer :
sig
  type param =
    {
      debug : bool;
      line_breaks : [`Unicode | `Traditional];
      block_splitting_threshold : int;
      tab_size : int;
      ansi : Ansi.param;
      marker : marker;
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
      marker : marker;
    }

  let line_number_width marked_source : int =
    let max_line_number_block Marked_source.{end_line_num; _} = end_line_num in
    let max_line_number_part Marked_source.{blocks; _} =
      Utils.maximum @@ List.map max_line_number_block blocks
    in
    let max_line_number (parts : _ Marked_source.t) =
      Utils.maximum @@ List.map max_line_number_part parts
    in
    String.length @@ Int.to_string @@ max_line_number marked_source

  let render_textloc ~param ~severity ~extra_remarks fmt (textloc : Loctext.t) =
    let located_tags, unlocated_tags =
      let main = `Main_message, textloc in
      let extra_remarks = List.mapi (fun i r -> `Extra_remark i, r) (Bwd.to_list extra_remarks) in
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
    let param = {
      Marked_source_renderer.severity = severity;
      tab_size = param.tab_size;
      line_number_width;
      ansi = param.ansi;
      marker = param.marker
    } in
    Marked_source_renderer.render ~param fmt marked_source;
    List.iter (render_unlocated_tag ~severity:param.severity ~ansi:param.ansi fmt) unlocated_tags

  let render_backtrace ~param ~severity fmt backtrace =
    indentf ~param:param.ansi fmt "%a"
      (fun fmt () -> List.iter (render_textloc ~param ~severity ~extra_remarks:Emp fmt) @@ Bwd.to_list backtrace) ()

  let render_diagnostic ~param fmt Diagnostic.{severity; message = short_code; explanation; backtrace; extra_remarks} =
    render_code ~severity ~param:param.ansi fmt short_code;
    render_backtrace ~param ~severity fmt backtrace;
    render_textloc ~param ~severity ~extra_remarks fmt explanation;
end

module Make (Message : Minimum_signatures.Message) =
struct
  let display ?(output=Stdlib.stdout) ?use_ansi ?use_color ?(show_backtrace=true) ?(marker=default_marker)
      ?(line_breaks=`Traditional) ?(block_splitting_threshold=5) ?(tab_size=8) ?(debug=false) d =
    let d = if show_backtrace then d else {d with Diagnostic.backtrace = Emp} in
    let d = Diagnostic.map Message.short_code d in
    let ansi = Ansi.Test.guess ?use_ansi ?use_color output in
    let param = {Diagnostic_renderer.debug; line_breaks; block_splitting_threshold; tab_size; ansi; marker} in
    let fmt = Format.formatter_of_out_channel output in
    Source_reader.run @@ fun () ->
    Diagnostic_renderer.render_diagnostic ~param fmt d;
    Format.pp_print_newline fmt ()
end
