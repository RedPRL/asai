(** @see <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions> *)

[@@@alert unstable
    "The GitHub Actions backend will likely change significantly in the future to account for more features."
]

(** The functor to create a printer for GitHub Actions workflow commands. *)
module Make (Code : Diagnostic.Code) : sig
  (** Print a diagnostic as a GitHub Actions workflow command.

      @see <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions> *)
  val print : Code.t Diagnostic.t -> unit
end
