(** Core of error reporting *)

(** {1 Introduction}

    asai provides a generic interface for generating and formatting messages for end users to see.
    Intended applications include compiler errors and warnings. *)

(** {1 Locations} *)

module Span = Span

(** {1 Diagnostics}

    A diagnostic is a message for the end user, for example a compiler warning or error. *)

module Diagnostic = Diagnostic

(** {1 Algebraic Effects}

    Algebraic effects to generate and handle diagnostics. When adopting asai, it is expected that
    one would use {!Logger.Make} to generate diagnostics. *)

module Logger = Logger

(** {1 Contextualization}

    These modules turn spans into highlighted code. *)

module Explicator = Explicator
