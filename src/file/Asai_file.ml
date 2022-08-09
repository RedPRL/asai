include Marked

open Bwd

module type S =
sig
  module Code : Asai.Code.S
  val format : splitting_threshold:int -> Code.t Asai.Diagnostic.t -> t
end

module Make (Code : Asai.Code.S) : S with module Code := Code
=
struct
  let format_files ~splitting_threshold ~additional_marks span =
    let marked_files =
      match span with
      | None -> Flatter.empty
      | Some sp -> Flatter.singleton (`Highlighted, sp)
    in
    let marked_files =
      (* add additional_marks *)
      List.fold_right (fun sp -> Flatter.add (`Marked, sp)) additional_marks marked_files
    in
    List.map Marker.mark_file @@ Flatter.flatten ~splitting_threshold marked_files

  let format_message ~splitting_threshold ~additional_marks (msg : _ Asai.Span.located) =
    format_files ~splitting_threshold ~additional_marks msg.loc, msg.value

  let format ~splitting_threshold (d : Code.t Asai.Diagnostic.t) =
    Reader.run @@ fun () ->
    { code = Code.to_string d.code
    ; severity = d.severity
    ; message = format_message ~splitting_threshold ~additional_marks:d.additional_marks d.message
    ; traces = Bwd.map (format_message ~splitting_threshold ~additional_marks:[]) d.traces
    }
end

module Internal =
struct
  module Reader = Reader
  module Flattened = Flattened
  module Flatter = Flatter
  module Marker = Marker
end
