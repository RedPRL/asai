open Bwd

module Marked = Marked

let assemble_sections ~splitting_threshold ~additional_marks span =
  let marked_sections =
    match span with
    | None -> Flattener.empty
    | Some sp -> Flattener.singleton (`Highlighted, sp)
  in
  let marked_sections =
    (* add additional_marks *)
    List.fold_right (fun sp -> Flattener.add (`Marked, sp)) additional_marks marked_sections
  in
  List.map Marker.mark_section @@ Flattener.flatten ~splitting_threshold marked_sections

let assemble_message ~splitting_threshold ~additional_marks (msg : _ Asai.Span.located) =
  assemble_sections ~splitting_threshold ~additional_marks msg.loc, msg.value

let assemble ~splitting_threshold (d : 'code Asai.Diagnostic.t) =
  Reader.run @@ fun () ->
  Marked.{
    code = d.code;
    severity = d.severity;
    message = assemble_message ~splitting_threshold ~additional_marks:d.additional_marks d.message;
    traces = Bwd.map (assemble_message ~splitting_threshold ~additional_marks:[]) d.traces;
  }

module Internal =
struct
  module Reader = Reader
  module Flattened = Flattened
  module Flattener = Flattener
  module Marker = Marker
end
