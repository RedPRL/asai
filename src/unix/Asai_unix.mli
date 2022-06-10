(** Diagnostic display for UNIX terminals. *)
open Asai

(** {1 Display}
    This module provides a single function for display Asai diagnostics to UNIX terminals. *)

module StringTbl := Hashtbl.Make(String)

module Make (ErrorCode : ErrorCode.S) : sig
  module Diagnostic := Diagnostic.Make(ErrorCode)
  val display : Diagnostic.display
end
