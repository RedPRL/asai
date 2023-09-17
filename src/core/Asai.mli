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

(** {1 Backends} *)

(** Experimental backend: GitHub Actions workflow commands. *)
module GitHub = GitHub
