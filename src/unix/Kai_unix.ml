open Bwd

module StringTbl = Hashtbl.Make(String)

open Kai
open Kai.Loc

open Notty
open Notty.Infix


module Make (ErrorCode : Kai.ErrorCode.S) =
struct
  module Diagnostic = Diagnostic.Make(ErrorCode)
  (* Error [E1]: An error happened! Oh no *)

  (*   🭁 file.cooltt *)
  (*   │  *)
  (* 1 │ λ x → let foo = x + y  *)
  (*   ┊       ─────────────── *)
  (* 2 │ in foo + z *)
  (*   ┊ ┬───────── *)
  (*   ┊ ╰╴ the message about the lines *)
  (*   🬂 *)

  let vline ~attr height str =
    I.vcat @@ List.init height (fun _ -> I.string ~attr str)

  let highlight_style (severity : Severity.t) =
    let open A in
    match severity with
    | Info -> fg green
    | Warning -> fg yellow
    | Error | Panic -> fg red

  let underline_style severity =
    A.st A.underline ++ highlight_style severity

  let fringe_style = A.(fg @@ gray 8)

  let render_cause ~buffers ~severity (cause : Diagnostic.cause) =
    let filename = Span.filename cause.location in

    let full_source = StringTbl.find buffers filename in
    let (before, highlighted, after) = Span.utf8_slice_lines full_source cause.location in
    let source =
      I.string before <|>
      I.string ~attr:(underline_style severity) highlighted <|>
      I.string after in

    let message =
      I.string ~attr:fringe_style "→ " <|> I.string ~attr:(highlight_style severity) cause.message
    in
    let message_lines =
      List.length @@ String.split_on_char '\n' cause.message
    in


    let body =
      I.string filename <->
      I.vpad 1 1 source <->
      message
    in

    let line_numbers =
      I.vpad 2 0 @@
      I.vcat @@
      List.map (fun n -> I.string ~attr:fringe_style @@ Int.to_string n) @@
      Span.line_numbers cause.location in
    let fringe_solid = vline ~attr:fringe_style (Span.height cause.location + 2) "│" in
    let fringe_dotted = vline ~attr:fringe_style message_lines "┊" in
    let fringe =
      I.string ~attr:fringe_style "🭁" <->
      fringe_solid <->
      fringe_dotted <->
      I.string ~attr:fringe_style "🬂"
    in
    I.hpad 1 1 line_numbers <|> fringe <|> I.hpad 1 0 body

  let display ~(buffers:string StringTbl.t) (diag : Diagnostic.t) =
    let severity = Diagnostic.severity diag in
    let header =
      I.vpad 0 1 @@
      I.string @@
      Format.asprintf "%a [%a%d]: %s"
        Severity.pp severity
        Severity.pp_short severity
        (ErrorCode.code_num diag.code)
        diag.message
    in
    let causes = List.map (render_cause ~buffers ~severity) @@ Bwd.to_list diag.causes in
    let image = I.vcat (header :: causes) in
    Notty_unix.output_image image;
    Printf.printf "\n";
end


