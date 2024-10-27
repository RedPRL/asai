(** Compiler diagnostics *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** {1 Core API} *)

(** Locations and ranges. *)
module Range = Range

(** Texts that preserve pretty-printing structures.

    @since 0.3.2 *)
module Text = Text

(** Located texts. "Loctext" is a portmanteau of "locate" and "text".

    @since 0.3.2 *)
module Loctext = Loctext

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. The API is optimized for attaching free-form text.

    @since 0.2.0 (renamed from Logger) *)
module Reporter = Reporter

(** Generating and handling diagnostics using algebraic effects. The API is optimized for fully structured messages.

    @since 0.2.0 *)
module StructuredReporter = StructuredReporter

(** Signatures that specify the minimum interface for libraries, applications, and handlers to work together.

    @since 0.3.0 *)
module MinimumSigs = MinimumSigs

(** {1 Experimental Diagnostic Handlers} *)

(** These handlers are subject to changes, but we will minimize incompatible changes between minor versions. *)

(** Diagnostic display for UNIX terminals. *)
module Tty = Tty

(** GitHub Actions workflow commands. *)
module GitHub = GitHub

(** {1 Internals} *)

(** The internals are exposed for convenience, but they are subject to changes between minor versions. *)

(** The definition of highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module MarkedSource = MarkedSource

(** Turning location information into highlighted text suitable for rendering. You probably do not need this module unless you want to create your own diagnostic handler. *)
module SourceMarker = SourceMarker

(** Reading the source content. It uses memory-mapped I/O for files. You probably do not need this module unless you want to create your own diagnostic handler. *)
module SourceReader = SourceReader

(**/**)

(** Helper functions for handling user content. This is exposed for internal testing. Absolutely no stability guarantees. *)
module SourceUtils = SourceUtils

(** The internal flattener that is tightly coupled with {!module:MarkedSource}. This is exposed for internal testing. Absolutely no stability guarantees. *)
module RangeFlattener = RangeFlattener
