open Bwd

open Asai

open Notty
open Notty.Infix

module Make (Code : Code.S) =
struct
  let vline attr height str =
    I.vcat @@ List.init height (fun _ -> I.string attr str)
  let column ~align images =
    let maxby f xs = List.(fold_left max 0 (map f xs)) in
    let width = maxby I.width images in
    List.map (I.hsnap ~align width) images |> I.vcat

  let highlight_style (severity : Severity.t) =
    let open A in
    st underline ++
    match severity with
    | Hint -> fg blue
    | Info -> fg green
    | Warning -> fg yellow
    | Error -> fg red
    | Bug -> bg red ++ fg black

  let marked_style = A.st A.underline

  let fringe_style = A.fg @@ A.gray 8

  let line_numbers_of_block ({start_line_num ; lines} : Asai_file.Marked.block) =
    column ~align:`Right @@
    List.map (fun n -> I.string fringe_style @@ Int.to_string n) @@
    List.init (List.length lines) (fun i -> start_line_num + i)

  let display_message code severity (sections, msg) =
    let segment (style, seg) =
      match style with
      (* TODO: how to display `Marked text? *)
      | None ->
        I.string A.empty seg
      | Some `Marked ->
        I.string marked_style seg
      | Some `Highlighted ->
        I.string (highlight_style severity) seg
    in
    let line segs =
      segs |> List.map segment |> I.hcat
    in
    let block (b : Asai_file.Marked.block) =
      (* We want to display the error message under whatever block contains the highlighted text *)
      (b.lines |> List.map line |> I.vcat) <->
      if List.exists (List.exists (function (Some `Highlighted,_) -> true | _ -> false)) b.lines then
        I.pad ~t:1 @@ I.strf "[%s] %t" (Code.to_string code) msg
      else
        I.void 0 0
    in
    let section ({file_path ; blocks} : Asai_file.Marked.section) =
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
    if sections = [] then
      I.strf "[%s] %t" (Code.to_string code) msg
    else
      (sections |> List.map (fun s -> s |> section |> I.pad ~b:1) |> I.vcat) |> I.crop ~b:2

  let display_marked debug (m : 'code Asai_file.Marked.t) =
    I.pad ~t:1 ~b:1 (display_message m.code m.severity m.message) <->
    if debug then
      I.pad ~b:1 (I.string A.empty ">>> Trace") <->
      (m.backtrace |> Bwd.map (fun t -> t |> display_message m.code m.severity |> I.pad ~b:1) |> Bwd.to_list |> List.rev |> I.vcat)
    else
      I.void 0 0

  module Assemble = Asai_file.Assembler.Make(Asai_file.FileReader)

  let display ?(display_traces = false) diag =
    let m = Assemble.assemble ~splitting_threshold:5 diag in
    Notty_unix.output_image (display_marked display_traces m)

  let interactive_trace diag =
    let m = Assemble.assemble ~splitting_threshold:5 diag in
    let traces =
      Bwd.append
        (m.backtrace |> Bwd.map (display_message m.code m.severity))
        [display_message m.code m.severity m.message] |> Bwd.to_list |> List.rev |> Array.of_list
    in
    let len = Array.length traces in
    let open Notty_unix in
    let rec loop t i =
      Term.image t (I.pad ~b:1 traces.(i) <->
                    I.strf "%d/%d" (i + 1) len <->
                    I.string A.empty "Use arrow keys to navigate up and down the stack trace" <->
                    I.string A.empty "Press Enter to Quit");
      match Term.event t with
      | `Key (`Arrow `Up, _) -> loop t (if i + 1 < len then i + 1 else i)
      | `Key (`Arrow `Down, _) -> loop t (if i - 1 >= 0 then i - 1 else i)
      | `Key (`Enter, _) -> ()
      | _ -> loop t i
    in
    let t = Term.create () in
    loop t 0;
    Term.release t
end