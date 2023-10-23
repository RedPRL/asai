(** @see <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions> *)

[@@@alert unstable
    "The GitHub Actions handler will likely change significantly in the future to account for more features."
]

(** The functor to create a printer for GitHub Actions workflow commands. *)
module Make (Message : Reporter.Message) : sig
  (** Print a diagnostic as a GitHub Actions workflow command. Only the main explanation will be printed; backtraces and extra remarks are ignored. Column numbers are also ignored because GitHub does not seem to use them.

      Example output:
      {v
::error file=examples/stlc/example.lambda,line=2,endLine=2,title=E002::Variable 'x' is not in scope
      v}

      @see <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions> *)
  val print : Message.t Diagnostic.t -> unit
end
