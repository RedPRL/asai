(** Core of message reporting *)

(** A diagnostic is a message for the end user, for example a compiler warning or error. *)

(** Locations and spans. *)
module Span = Span

(** The definition of diagnostics and some utility functions. *)
module Diagnostic = Diagnostic

(** Generating and handling diagnostics using algebraic effects. *)
module Logger = Logger

(** Turning location information into highlighted text suitable for rendering. *)
module Explicator = Explicator

(** Experimental backend: GitHub Actions workflow commands. *)
module GitHub = GitHub
