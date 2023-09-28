[@@@alert unstable
    "The TTY backend will likely change significantly in the future to account for more features."
]

(** {1 Display} *)

(** This module provides functions to display or interact with diagnostics in UNIX terminals. A message will look like this:

    {v
    🭁 examples/stlc/example.lambda
    │
  1 │ (check (λ ä (λ 123
  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
    ┊
 20 │ ahhhhhhhhhhhhhhhhhh
 21 │ noooooooooooooooooo
    ┷
 [E002] Why am I checking the term (→ ℕ (→ ℕ ℕ)),
        which looks amazing?
    v}
*)
module Make (Code : Diagnostic.Code) : sig

  (** [display d] prints the diagnostic [d] to the standard output, using terminal control characters for formatting. A message will look like this:

      {v
    🭁 examples/stlc/example1.lambda
    │
  1 │ (check (λ ä (λ 123
  2 │   sdaf)) (→ ℕ (→ ℕ ℕ)))
    ┊
 20 │ ahhhhhhhhhhhhhhhhhh
 21 │ noooooooooooooooooo
    ┷
 [E002] Why am I checking the term (→ ℕ (→ ℕ ℕ)),
        which looks amazing?

    🭁 examples/stlc/example2.lambda
    │
  3 │ let x = 1 // additional information
  4 │ let y = 1
    ┷
    🭁 examples/stlc/example3.lambda
    │
  8 │ assert (asai is cool)
    ┷
      v}

      @param show_backtrace Whether the backtrace should be shown. The default is [false].
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].

      @raise Invalid_argument if `tab_size < 0`.
  *)
  val display : ?show_backtrace:bool -> ?tab_size:int -> Code.t Diagnostic.t -> unit

  (** [interactive_trace d] drops the user in a small interactive terminal app where they can cycle through the message provided in [d] and its backtrace.

      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].

      @raise Invalid_argument if `tab_size < 0`.
  *)
  val interactive_trace : ?tab_size:int -> Code.t Diagnostic.t -> unit
end
