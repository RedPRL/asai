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

  let underline_style (severity : Severity.t) =
    let open A in
    match severity with
    | Info -> st underline ++ fg green
    | Warning -> st underline ++ fg yellow
    | Error | Panic -> st underline ++ fg red

  let render_cause ~buffers ~severity (cause : Diagnostic.cause) =
    let filename = Span.filename cause.location in

    let full_source = StringTbl.find buffers filename in
    let (before, highlighted, after) = Span.utf8_slice_lines full_source cause.location in
    let source =
      I.string before <|>
      I.string ~attr:(underline_style severity) highlighted <|>
      I.string after in

    let body = I.string filename <-> I.vpad 1 1 source in

    let line_numbers =
      I.vsnap (2 + Span.height cause.location) @@
      I.vcat @@
      List.map (fun n -> I.string @@ Int.to_string n) @@
      Span.line_numbers cause.location in

    let fringe_center = I.vcat @@ List.init (Span.height cause.location + 2) (fun _ -> I.string "│") in
    let fringe = I.string "🭁" <-> fringe_center <-> I.string "🬂" in
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


