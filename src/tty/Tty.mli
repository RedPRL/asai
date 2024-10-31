[@@@alert unstable
    "The TTY handler will likely change significantly in the future to account for more features."
]

(** {1 TTY backend} *)

(** {2 Custom markers} *)

(** The type of custom marker functions. A marker function takes the following three arguments:
    + [ansi]: whether ANSI escape sequences are enabled, and if so, whether colors are used.
    + The mark's target message: [`Main_message] is the main message, and [`Extra_remark i] is the [i]th extra remark in the diagnostic.
    + Whether the mark indicates the start or end of a non-empty range, or the location of a point (an empty range), and whether the mark is at the end of a line or a file.
    The output is the string to visualize the mark within the source text.

    @since 0.4.0 *)
type marker =
  ansi:[ `Enabled_with_color | `Enabled_without_color | `Disabled ]
  -> [ `Main_message | `Extra_remark of int ]
  -> [ `Range_begin
     | `Range_end of [`End_of_line | `End_of_file] option
     | `Point of [`End_of_line | `End_of_file] option
     ]
  -> string

(** The default marker. Currently, it visualizes point marks as ‹POS›, ‹EOL›, or ‹EOF› (depending on whether they are at the end of a line or a file) and range marks as "«" and "»" when ANSI escape sequences are disabled.

    @since 0.4.0 *)
val default_marker : marker

(** {2 Display function} *)

(** This module provides functions to display diagnostics in terminals. *)
module Make (Message : Minimum_signatures.Message) : sig

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
      @param use_ansi Whether to use ANSI escape sequences, overriding auto-detection. ANSI escape sequences are used for coloring and styling the output. By default, auto-detection checks if the [output] is a TTY and if the environment variable [TERM] is set to a non-empty value other than [dumb].
      @param use_color Whether to use colors when ANSI escape sequences are used, overriding auto-detection. By default, auto-detection will disable colors if ANSI escape sequences are not used or if the environment variable [NO_COLOR] is set to a non-empty value. Note that even when colors are disabled, the handler may still use bold, faint, and underline styles for highlighting if ANSI escape sequences are used. It is an error to explicitly set [use_color] to [true] and [use_ansi] to [false].
      @param show_backtrace Whether the backtrace should be shown. The default is [true].
      @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is [5].
      @param marker A function that displays marks as strings. It takes the final values of [use_ansi] and [use_color], the target message the mark belongs to, and whether the mark indicates the start or end of a range or the location of a point. See {!type:marker} for more details. The default value is {!val:default_marker}.
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].
      @param debug Whether to enable the debug mode that performs expensive extra checking. The default is [false].

      @raise Invalid_argument if [use_color] is explicitly set to [true] but [use_ansi] is explicitly set to [false], or if [tab_size < 0], or if invalid ranges are detected. When the debug mode is enabled, detection of invalid ranges will raise the more structured exception {!exception:Source_marker.Invalid_range} instead.
      @raise Source_marker.Invalid_range if the debug mode is enabled and invalid ranges are detected. See {!exception:Source_marker.Invalid_range} for the detailed listing of all possible errors being reported.

      @before 0.4.0 The optional parameter [marker] was not present, and angle quotation marks were not used even when ANSI escape sequences are not used.
      @see <https://no-color.org/> for the [NO_COLOR] specification
  *)
  val display : ?output:out_channel -> ?use_ansi:bool -> ?use_color:bool -> ?show_backtrace:bool -> ?line_breaks:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> ?marker:marker -> ?tab_size:int -> ?debug:bool -> Message.t Diagnostic.t -> unit
end
