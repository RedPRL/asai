(** Compiler diagnostics *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** {1 Core API} *)

(** Locations and spans. *)
module Span = Span

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. *)
module Logger = Logger

(** Turning location information into highlighted text suitable for rendering. *)
module Explicator = Explicator

(** {1 Experimental Backends} *)

(** Diagnostic display for UNIX terminals. *)
module Tty = Tty

(** An LSP (Language Service Protocol) server for asai *)
module Lsp = AsaiLsp

(** GitHub Actions workflow commands. *)
module GitHub = GitHub
