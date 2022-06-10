module StringTbl := Hashtbl.Make(String)

open Asai

module Make (ErrorCode : ErrorCode.S) : sig
  module Diagnostic := Diagnostic.Make(ErrorCode)
  val display : Diagnostic.display
end
