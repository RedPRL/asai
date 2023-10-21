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

(** These handlers are subject to changes, but we will minimize incompatible changes between minor versions. *)

(** Diagnostic display for UNIX terminals. *)
module Tty = Tty

(** GitHub Actions workflow commands. *)
module GitHub = GitHub

(** {1 Internals} *)

(** The internals are exposed for convenience, but they are subject to changes between minor versions. *)

(** The definition of highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Explication = Explication

(** Turning location information into highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Explicator = Explicator

(** Reading the source content. It uses memory-mapped I/O for files. You probably do not need this module unless you want to create your own diagnostic handler. *)
module SourceReader = SourceReader

(**/**)

(** Backward compatibility *)
module Logger = Reporter [@@ocaml.alert deprecated "Use Reporter instead"]

(** Helper functions for handling user content. This is exposed for internal testing. Absolutely no stability guarantees. *)
module UserContent = UserContent

(** The internal flattener that is tightly coupled with {!module:Explication}. This is exposed for internal testing. Absolutely no stability guarantees. *)
module Flattener = Flattener
