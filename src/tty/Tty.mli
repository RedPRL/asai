[@@@alert unstable
    "The TTY handler will likely change significantly in the future to account for more features."
]

(** {1 Display} *)

(** This module provides functions to display or interact with diagnostics in UNIX terminals. *)
module Make (Message : MinimumSigs.Message) : sig

  (** [display d] prints the diagnostic [d] to the standard output, using terminal control characters for formatting. A message will look like this (but with coloring):

      {v
 ￫ warning[hello]
 ╭ ￭ /path/to/file.cool
 ┆ 1 | aaaaaaaaaa
 ┆ 2 | bbbbbbbbbb
 ╯   ^ when stepping into the abyss
 ￭ /path/to/file.cool
 2 | bbbbbbbbbb
 3 | cccccccccc
   ^ could not say hi here
      v}

      @param output The output channel, such as {!val:stdout} or {!val:stderr}. By default, it is {!val:stdout}, the standard output.
      @param use_ansi Whether ANSI escape sequences should be used, overwriting the auto-detection. By default, the auto-detection checks whether the [output] is a TTY and whether the environment variable [TERM] is set to a non-empty value other than [dumb]. Note that this handler is currently using {i exclusively} ANSI escape sequences for highlighting, which means turning them off will lose the precise location. (This may change in the future.)
      @param use_color Whether colors should be use when ANSI escape sequences are used, overwriting the auto-detection. By default, the auto-detection will turn off the colors if a non-empty value was assigned to the environment variable [NO_COLOR]. Note that even when the colors are turned off, the handler may still use the bold style, the faint style, and underlines for highlighting. This parameter has no effects if ANSI escape sequences are not used.
      @param show_backtrace Whether the backtrace should be shown. The default is [true].
      @param line_breaking The algorithm to recognize (hard) line breaks. The [`Unicode] algorithm recognizes all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1} as line breaks. The [`Traditional] algorithm only recognizes [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] algorithm.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is [5].
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].

      @raise Invalid_argument if [tab_size < 0].

      @see <https://no-color.org/> for the [NO_COLOR] specification
  *)
  val display : ?output:out_channel -> ?use_ansi:bool -> ?use_color:bool -> ?show_backtrace:bool -> ?line_breaking:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> ?tab_size:int -> Message.t Diagnostic.t -> unit
end
