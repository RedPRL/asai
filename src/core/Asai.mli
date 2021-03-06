(** Diagnostics and other span-associated metadata.*)

(** {1 Introduction}
    When we write any sort of tool that processes source code (compiler, interpreter, etc),
    we produce a large amount of information that needs to be associated with some span
    in the original source code. The most obvious case is error messages and other diagnostics,
    but this also includes types, identifier provenance, and so on. Managing this information
    is an annoying, repetitive, and error-prone task, which is exactly the problem Asai aims to
    solve. *)

(** {1 Location Types}
    As one would expect from a library that handles information associated with source-code
    positions, we provide a set of position types. These are defined as abstract types to
    ensure that proper care is taken when working with the more difficult bits of Unicode. *)
open Loc
module Pos = Pos
module Span = Span
module Loc = Loc

(** {1 Diagnostics}
    Good diagnostics are a large part of what makes good tools, so we take special care to
    handle these specially. In Asai, all diagnostics are parameterized by an [ErrorCode],
    which is intended to be a sum type representing all the high-level error categories
    that your tool may produce. *)

module ErrorCode = ErrorCode
module Severity = Severity
module Diagnostic = Diagnostic

(** {1 Effects}
    Managing the current span/position can become tedious, so Asai provides a series of effects for
    handling this. *)
module Effects = Effects
