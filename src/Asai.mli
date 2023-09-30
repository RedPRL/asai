(** Compiler diagnostics *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** {1 Core API} *)

(** Locations and spans. *)
module Span = Span

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. *)
module Logger = Logger

(** {1 Experimental Backends} *)

(** Diagnostic display for UNIX terminals. *)
module Tty = Tty

(** An LSP (Language Service Protocol) server for asai *)
module Lsp = AsaiLsp

(** GitHub Actions workflow commands. *)
module GitHub = GitHub

(** {1 Internals} *)

(** The definition of highlighted text suitable for rendering. You probably do not need this module unless you want to create your own backend. *)
module Explication = Explication

(** Turning location information into highlighted text suitable for rendering. You probably do not need this module unless you want to create your own backend. *)
module Explicator = Explicator

(** An implementation of {!module-type:Explicator.Reader} using memory-mapped file I/O. You probably do not need this module unless you want to create your own backend. *)
module FileReader = FileReader

(**/**)

(** Helper functions for handling user content. *)
module UserContent = UserContent
