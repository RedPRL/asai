(** Compiler diagnostics *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** {1 Core API} *)

(** Locations and spans. *)
module Span = Span

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. The API is optimized for attaching free-form text.

    @since 0.2.0 (renamed from Logger) *)
module Reporter = Reporter

(** Generating and handling diagnostics using algebraic effects. The API is optimized for fully structured messages.

    @since 0.2.0 *)
module StructuredReporter = StructuredReporter

(** {1 Experimental Diagnostic Handlers} *)

(** Diagnostic display for UNIX terminals. *)
module Tty = Tty

(** An LSP (Language Service Protocol) server for asai *)
module Lsp = AsaiLsp

(** GitHub Actions workflow commands. *)
module GitHub = GitHub

(** {1 Internals} *)

(** The definition of highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Explication = Explication

(** Turning location information into highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Explicator = Explicator

(** Reading the source content. It uses memory-mapped I/O for files. You probably do not need this module unless you want to create your own diagnostic handler. *)
module SourceReader = SourceReader

(**/**)

(** Backward compatibility *)
module Logger = Reporter [@@ocaml.alert deprecated "Use Reporter instead"]

(** Helper functions for handling user content. *)
module UserContent = UserContent
