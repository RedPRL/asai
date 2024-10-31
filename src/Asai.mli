(** Compiler diagnostics *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** {1 Core API} *)

(** Positions and ranges in a string or a file. *)
module Range = Range

(** Texts that preserve pretty-printing structures.

    @since 0.4.0 *)
module Text = Text

(** Located texts. "Loctext" is a portmanteau of "locate" and "text".

    @since 0.4.0 *)
module Loctext = Loctext

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. The API is optimized for attaching free-form text.

    @since 0.2.0 (renamed from Logger) *)
module Reporter = Reporter

(** Generating and handling diagnostics using algebraic effects. The API is optimized for fully structured messages.

    @since 0.4.0 (renamed from StructuredReporter) *)
module Structured_reporter = Structured_reporter

(** Signatures that specify the minimum interface for libraries, applications, and handlers to work together.

    @since 0.4.0 (renamed from MinimumSigs) *)
module Minimum_signatures = Minimum_signatures

(** {1 Experimental Diagnostic Handlers} *)

(** These handlers are subject to changes, but we will minimize incompatible changes between minor versions. *)

(** Diagnostic display for terminals. *)
module Tty = Tty

(** GitHub Actions workflow commands. *)
module Github = Github

(** {1 Internals} *)

(** The internals are exposed for convenience, but they are subject to changes between minor versions. *)

(** The definition of highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Marked_source = Marked_source

(** Turning location information into highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Source_marker = Source_marker

(** Reading the source content. It uses memory-mapped I/O for files. You probably do not need this module unless you want to create your own diagnostic handler. *)
module Source_reader = Source_reader

(**/**)

(** Backward compatibility *)
module StructuredReporter = Structured_reporter [@@ocaml.alert deprecated "Use Asai.Structured_reporter instead"]

(** Backward compatibility *)
module MinimumSigs = Minimum_signatures [@@ocaml.alert deprecated "Use Asai.Minimum_signatures instead"]

(** Helper functions for handling user content. This is exposed for internal testing. Absolutely no stability guarantees. *)
module String_utils = String_utils

(** The internal flattener that is tightly coupled with {!module:Marked_source}. This is exposed for internal testing. Absolutely no stability guarantees. *)
module Range_flattener = Range_flattener
