open Bwd

module type S =
sig
  val assemble : splitting_threshold:int -> ('code,'phase) Asai.Diagnostic.t -> ('code,'phase) Marked.t
end

module Make (R : Reader.S) =
struct
  module M = Marker.Make (R)

  let sections ~splitting_threshold ~additional_marks span =
    let marked_sections =
      match span with
      | None -> Flattener.empty
      | Some sp -> Flattener.singleton (`Highlighted, sp)
    in
    let marked_sections =
      (* add additional_marks *)
      List.fold_right (fun sp -> Flattener.add (`Marked, sp)) additional_marks marked_sections
    in
    List.map M.mark_section @@ Flattener.flatten ~splitting_threshold marked_sections

  let message ~splitting_threshold ~additional_marks (msg : _ Asai.Span.located) =
    sections ~splitting_threshold ~additional_marks msg.loc, msg.value

  let assemble ~splitting_threshold (d : ('code,'phase) Asai.Diagnostic.t) =
    R.run @@ fun () ->
    Marked.{
      code = d.code;
      severity = d.severity;
      message = message ~splitting_threshold ~additional_marks:d.additional_marks d.message;
      backtrace = Bwd.map (fun (p,t) -> (p,message ~splitting_threshold ~additional_marks:[] t)) d.backtrace;
    }
end
