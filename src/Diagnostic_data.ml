open Bwd

(** The type of severity. *)
type severity =
  | Hint (** This corresponds to the [Hint] severity level from LSP (Language Server Protocol). The official specification did not give much guidance on the difference between this level and {!constructor:Info}. However, {{: https://github.com/microsoft/language-server-protocol/issues/325#issuecomment-344570720}according to the LSP developers, "the idea of the hint severity" is that "for example we in VS Code don't render in the UI as squiggles."} They are often used to indicate code smell, along with edit suggestions to fix it. *)
  | Info (** This corresponds to the [Information] severity level from LSP (Language Server Protocol). The official specification did not give much guidance on the difference between this level and {!constructor:Hint}. *)
  | Warning (** Something went wrong or looked suspicious, but the end user (the user of your proof assistant or compiler) may choose to ignore the issue. For example, maybe some named arguments were not used in a definition. *)
  | Error (** A serious error caused by the end user (the user of your proof assistant or compiler) or other external factors (e.g., internet not working). *)
  | Bug (** A serious error likely caused by a bug in the proof assistant. You would want the end user to report the bug back to you. This is useful for indicating that certain branches in a pattern matching should be "impossible", while printing out debugging information in case the program logic is flawed. *)

(** The type of diagnostics. *)
type ('message, 'loctext) t = {
  severity : severity;
  (** Severity of the diagnostic. *)
  message : 'message;
  (** The (structured) message. *)
  explanation : 'loctext;
  (** The free-form explanation. *)
  backtrace : 'loctext bwd;
  (** The backtrace leading to this diagnostic. *)
  extra_remarks : 'loctext bwd;
  (** Additional remarks that are relevant to the main message but not part of the backtrace. It is a backward list so that new remarks can be added to its end easily. *)
}
