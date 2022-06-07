open Bwd
open Loc

module StringMap = Map.Make(String)

module type S =
sig
  type t
  type code
  type cause
  type highlight

  val build : code:code -> string -> t
  val with_cause : location:Span.t -> t -> t
  val with_highlight : location:Span.t -> message:string -> t -> t

  val pp : string StringMap.t -> Format.formatter -> t -> unit
end

module Make (ErrorCode : ErrorCode.S) : S with type code = ErrorCode.t =
struct
  type highlight =
      Highlight of {
        location : Span.t;
        message : string
      }

  type cause =
      Cause of {
        location : Span.t;
        highlight : highlight option
      }

  type t = 
      Diagnostic of {
        message : string;
        (** The main message of a diagnostic. *)
        code : ErrorCode.t;
        (** The error code associated with a diagnostic. *)
        causes : cause bwd
      }

  type code = ErrorCode.t

  let build ~code message =
    Diagnostic {
      message;
      code;
      causes = Emp
    }

  let with_cause ~location (Diagnostic diag) =
    let cause = Cause { location = location; highlight = None } in
    Diagnostic { diag with causes = Snoc(diag.causes, cause) }

  let with_highlight ~location ~message (Diagnostic diag) =
    match diag.causes with
    | Snoc(causes, Cause cause) ->
      let highlight = Some (Highlight {location; message}) in
      Diagnostic { diag with causes = Snoc(causes, Cause { cause with highlight}) }
    | Emp ->
      Diagnostic diag

  let pp_highlight fmt (Highlight highlight) =
    (* Print the underline. *)
    Format.pp_print_string fmt "  • ";
    for _ = 0 to Span.start_col highlight.location - 1 do
      Format.pp_print_space fmt ();
    done;
    Format.pp_print_string fmt "┬";
    for _ = 0 to Span.width highlight.location - 2 do
      Format.pp_print_string fmt "─";
    done;
    (* Print the message *)
    Format.pp_force_newline fmt ();
    Format.pp_print_string fmt "  • ";
    for _ = 0 to Span.start_col highlight.location - 1 do
      Format.pp_print_space fmt ();
    done;
    Format.pp_print_string fmt "╰╸ ";
    Format.pp_print_string fmt highlight.message

  let pp_cause file_contents fmt (Cause cause) =
    (* [TODO: Reed M, 07/06/2022] Properly handle multi-line highlights! *)
    let highlight_line =
      match cause.highlight with
      | Some (Highlight { location = Span sp;_ }) -> sp.start_line_num
      | None -> -1
    in
    let (Span location) = cause.location in
    let lines =
      Span.slice (StringMap.find location.filename file_contents) cause.location
      |> String.split_on_char '\n'
    in

    Format.pp_print_string fmt "  ╭──▶ ";
    Span.pp fmt cause.location;
    Format.pp_force_newline fmt ();
    Format.pp_print_string fmt "  │ ";
    Format.pp_force_newline fmt ();
    (* Print out the source + highlights. *)
    for i = location.start_line_num to location.stop_line_num do
      Format.pp_print_int fmt i;
      Format.pp_print_space fmt ();
      Format.pp_print_string fmt "│ ";
      Format.pp_print_string fmt (List.nth lines (i - location.start_line_num));
      if i = highlight_line then begin
        Format.pp_force_newline fmt ();
        Format.pp_print_option pp_highlight fmt cause.highlight;
        Format.pp_force_newline fmt ();
      end
    done

  let pp file_contents fmt (Diagnostic diag) =
    let pp_sep fmt () =
      Format.pp_print_newline fmt ()
    in
    let sev = ErrorCode.severity diag.code in
    Format.fprintf fmt "%a [%a%u]: %s@.@[<h>%a@]"
      Severity.pp sev
      Severity.pp_short sev
      (ErrorCode.code_num diag.code)
      diag.message
      (Format.pp_print_list ~pp_sep (pp_cause file_contents)) @@ Bwd.to_list diag.causes
end
