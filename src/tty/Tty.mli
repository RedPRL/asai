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

      @param line_breaking The algorithm to recognize (hard) line breaks. The [`Unicode] algorithm recognizes all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1} as line breaks. The [`Traditional] algorithm only recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] algorithm.
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].
      @param show_backtrace Whether the backtrace should be shown. The default is [false].

      @raise Invalid_argument if `tab_size < 0`.
  *)
  val display : ?line_breaking:[`Unicode | `Traditional] -> ?tab_size:int -> ?show_backtrace:bool -> Code.t Diagnostic.t -> unit

  (** [interactive_trace d] drops the user in a small interactive terminal app where they can cycle through the message provided in [d] and its backtrace.

      @param line_breaking The algorithm to recognize (hard) line breaks. The [`Unicode] algorithm recognizes all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1} as line breaks. The [`Traditional] algorithm only recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] algorithm.
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].

      @raise Invalid_argument if `tab_size < 0`.
  *)
  val interactive_trace : ?line_breaking:[`Unicode | `Traditional] -> ?tab_size:int -> Code.t Diagnostic.t -> unit
end
