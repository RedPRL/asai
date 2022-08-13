(** Read files and flatten spans into marked text. *)

(** Definition of marked text. *)
module Marked = Marked

(** Generic content reader. *)
module Reader = Reader

(** File reader. *)
module FileReader = FileReader

(** Assembler. *)
module Assembler = Assembler

(**/**)

module Internal :
sig
  module Flattened = Flattened
  module Flattener = Flattener
  module Marker = Marker
end
