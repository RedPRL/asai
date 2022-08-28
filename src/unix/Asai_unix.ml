open Bwd

open Asai

open Notty
open Notty.Infix

module Make (Code : Code.S) =
struct
  let vline ~attr height str =
    I.vcat @@ List.init height (fun _ -> I.string ~attr str)
  let column ~align images =
    let maxby f xs = List.(fold_left max 0 (map f xs)) in
    let width = maxby I.width images in
    List.map (I.hsnap ~align width) images |> I.vcat

  let highlight_style (severity : Severity.t) =
    let open A in
    match severity with
    | Info -> fg green
    | Warning -> fg yellow
    | Error | InternalError -> fg red

  let underline_style severity =
    A.st A.underline ++ highlight_style severity

  let fringe_style = A.(fg @@ gray 8)

  let line_numbers_of_block ({start_line_num ; lines} : Asai_file.Marked.block) = 
    column ~align:`Right @@
    List.map (fun n -> I.string ~attr:fringe_style @@ Int.to_string n) @@
    List.init (List.length lines) (fun i -> start_line_num + i)

  let marked ({code = _; severity; message = msg; traces} : 'code Asai_file.Marked.t) =
    let segment (style,seg) = 
      match style with
        (* TODO: how to display `Marked text? *)
        | None | Some `Marked -> I.string seg
        | Some `Highlighted -> I.string ~attr:(underline_style severity) seg
    in
    let line segs =
      segs |> List.map segment |> I.hcat 
    in
    let block (b : Asai_file.Marked.block) =
      b.lines |> List.map line |> I.vcat
    in
    let section ({file_path ; blocks} : Asai_file.Marked.section) =
      let line_numbers = blocks |> List.map line_numbers_of_block in 
      let fringes = line_numbers |> List.map (fun img -> vline ~attr:fringe_style (I.height img) "│") in
      let line_numbers = line_numbers |> List.map (I.vpad 0 2) |> column ~align:`Right |> I.vcrop 0 2 in
      let fringe = 
        I.string ~attr:fringe_style ("🭁") <->
        I.string ~attr:fringe_style "│" <->
        (fringes |> List.map (fun img -> img <-> vline ~attr:fringe_style 2 "┊") |> I.vcat |> I.vcrop 0 2) <->
        I.string ~attr:fringe_style "│" <->
        I.string ~attr:fringe_style "🭁"
      in
      let side_panel = I.vpad 2 0 line_numbers <|> fringe in
      let blocks = blocks |> List.map (fun b -> block b |> I.vpad 0 2) |> I.vcat in
      let body = I.vpad 0 1 (I.string file_path) <-> blocks in
      (I.hpad 0 1 side_panel <|> body) |> I.vpad 0 1
    in
    let message (sections,msg) =
      let header =
        I.vpad 0 1 @@
        I.strf "%a: %t"
          Severity.pp severity
          msg
      in
      header <->
      (sections |> List.map (fun s -> s |> section |> I.vpad 0 2) |> I.vcat) |> I.vcrop 0 2
    in
    I.vpad 1 1 (message msg) <->
    I.string "Trace" <->
    I.string "---------------------------------------------" <->
    I.string "" <->
    (traces |> Bwd.map (fun t -> t |> message |> I.vpad 0 1) |> Bwd.to_list |> List.rev |> I.vcat)


  module Assemble = Asai_file.Assembler.Make(Asai_file.FileReader)

  let display diag =
    let m = Assemble.assemble ~splitting_threshold:5 diag in
    Notty_unix.output_image (marked m)
end