open Bwd
open Notty
open Notty.Infix

module Style = struct
  type t = HighlightedPrimary | HighlightedAdditional | Primary | Additional | None
  let none = None
  let is_none x = x = None
  let equal (x : t) y = x = y
  let compare (x : t) (y : t) = - Stdlib.compare x y
  let max x y = Stdlib.min x y
  let compose x y = max x y
end

module E = Explicator.Make(FileReader)(Style)

module Make (Code : Diagnostic.Code) =
struct
  let vline attr height str =
    I.vcat @@ List.init height (fun _ -> I.string attr str)
  let column ~align images =
    let maxby f xs = List.(fold_left max 0 (map f xs)) in
    let width = maxby I.width images in
    List.map (I.hsnap ~align width) images |> I.vcat

  let attr (severity : Diagnostic.severity) (style : Style.t) =
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

  let marked_style = A.st A.underline

  let fringe_style = A.fg @@ A.gray 8

  let line_numbers_of_block Explicator.{start_line_num; lines} =
    column ~align:`Right @@
    List.map (fun n -> I.string fringe_style @@ Int.to_string n) @@
    List.init (List.length lines) (fun i -> start_line_num + i)

  let display_parts code severity text parts =
    if parts = [] then
      I.strf "[%s] %t" (Code.to_string code) text
    else
      let segment Explicator.{style; value = seg} =
        I.string (attr severity style) seg
      in
      let line (segs : _ Explicator.line) =
        segs |> List.map segment |> I.hcat
      in
      let block (b : _ Explicator.block) =
        (* We want to display the error message under whatever block contains the highlighted text *)
        (b.lines |> List.map line |> I.vcat) <->
        if List.exists (List.exists (function Explicator.{style = (Style.Primary | HighlightedPrimary); _} -> true | _ -> false)) b.lines then
          I.pad ~t:1 @@ I.strf "[%s] %t" (Code.to_string code) text
        else
          I.void 0 0
      in
      let part Explicator.{file_path; blocks} =
        let line_numbers = blocks |> List.map line_numbers_of_block in
        let fringes = line_numbers |> List.map (fun img -> vline fringe_style (I.height img) "│") in
        let line_numbers = line_numbers |> List.map (I.pad ~b:2) |> column ~align:`Right |> I.crop ~b:2 in
        let fringe =
          I.string fringe_style ("🭁") <->
          I.string fringe_style "│" <->
          (fringes |> List.map (fun img -> img <-> vline fringe_style 2 "┊") |> I.vcat |> I.crop ~b:2) <->
          I.string fringe_style "│" <->
          I.string fringe_style "█"
        in
        let side_panel = I.pad ~t:2 ~l:1 ~r:1 line_numbers <|> fringe in
        let blocks = blocks |> List.map (fun b -> block b |> I.pad ~b:1) |> I.vcat in
        let body = I.pad ~b:1 (I.string A.empty file_path) <-> blocks in
        I.pad ~r:1 side_panel <|> body
      in
      parts |> List.map (fun s -> s |> part |> I.pad ~b:1) |> I.vcat |> I.crop ~b:2

  let display_message ?(additional_messages : Diagnostic.message list = []) code severity (msg : Diagnostic.message)  =
    let style s x = Explicator.{value = x; style = s} in
    let main_span = Option.to_list @@ Option.map (style Style.HighlightedPrimary) msg.loc in
    let additional_spans = List.filter_map (fun x -> Option.map (style Style.Additional) x.Span.loc) additional_messages in
    let parts = E.explicate ~splitting_threshold:5 (main_span @ additional_spans) in
    display_parts code severity msg.value parts

  let display_diagnostic show_backtrace Diagnostic.{code; severity; message; additional_messages; backtrace} =
    FileReader.run @@ fun () ->
    begin
      if show_backtrace then
        I.pad ~b:1 (I.string A.empty ">>> Trace") <->
        (backtrace |> Bwd.map (fun t -> t |> display_message code severity |> I.pad ~b:1) |> Bwd.to_list |> I.vcat)
      else
        I.void 0 0
    end
    <->
    I.pad ~t:1 ~b:1 (display_message code severity message ~additional_messages)

  module F = Explicator.Make(FileReader)

  let display ?(backtrace = false) diag =
    Notty_unix.output_image (display_diagnostic backtrace diag)

  let interactive_trace Diagnostic.{code; severity; message; additional_messages; backtrace} =
    let traces =
      FileReader.run @@ fun () ->
      Bwd.append
        (backtrace |> Bwd.map (display_message code severity))
        [display_message code severity message ~additional_messages] |> Bwd.to_list |> Array.of_list
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
