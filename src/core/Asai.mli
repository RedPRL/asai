(** Core of error reporting *)

(** {1 Positions, Spans, and Locations} *)

module Span = Span

(** {1 Diagnostics}

    A diagnostic is a message for the end user, for example a compiler warning or error. *)

module Diagnostic = Diagnostic

(** {1 Creating Diagnostics}

    Generating and handling diagnostics using algebraic effects. *)

module Logger = Logger

(** {1 Explication}

    Turning {!type:Span.t} into highlighted code suitable for rendering. *)

module Explicator = Explicator
