[@@@alert unstable
    "The TTY handler will likely change significantly in the future to account for more features."
]

(** {1 TTY backend} *)

(** {2 Custom markers} *)

(** The type of custom marker functions. The output is the string to visualize the mark within the source text. *)
type marker =
  ansi:[ `Enabled_with_color | `Enabled_without_color | `Disabled ]
  -> [ `Main_message | `Extra_remark of int ]
  -> [ `Range_begin
     | `Range_end of [`End_of_line | `End_of_file] option
     | `Point of [`End_of_line | `End_of_file] option
     ]
  -> string

(** The default marker. Currently, it transforms point marks into [‹POS›], [‹EOL›], or [‹EOF›] (depending on whether they are at the end of a line or a file) and ignores all range marks. This function is subject to change; future versions may display range marks when [use_ansi] is false.

    @since 0.4.0 *)
val default_marker : marker

(** {2 Display function} *)

(** This module provides functions to display or interact with diagnostics in UNIX terminals. *)
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
      @param use_ansi Whether ANSI escape sequences should be used, overwriting the auto-detection. By default, the auto-detection checks whether the [output] is a TTY and whether the environment variable [TERM] is set to a non-empty value other than [dumb]. Note that this handler is currently using {i exclusively} ANSI escape sequences for highlighting, which means turning them off will lose the precise location. (This may change in the future.)
      @param use_color Whether colors should be use when ANSI escape sequences are used, overwriting the auto-detection. By default, the auto-detection will turn off the colors if ANSI escape sequences should not be used or if a non-empty value was assigned to the environment variable [NO_COLOR]. Note that even when the colors are turned off, the handler may still use the bold style, the faint style, and underlines for highlighting if ANSI escape sequences are used. It is an error to pass [true] as this parameter and [false] as [use_ansi].
      @param show_backtrace Whether the backtrace should be shown. The default is [true].
      @param marker A function that that displays markers as strings. It takes the final values of [use_ansi] and [use_color], an indication whether it is at the end of a line or a file, and the marker to visualize. See {!type:marker}. The default value is {!val:default_marker}.
      @param line_breaks The set of character sequences that are recognized as (hard) line breaks. The [`Unicode] set contains all Unicode character sequences in {{:https://www.unicode.org/versions/Unicode15.0.0/ch05.pdf#G41643}Unicode 15.0.0 Table 5-1.} The [`Traditional] set only contains [U+000A (LF)], [U+000D (CR)], and [U+000D U+000A (CRLF)] as line breaks. The default is the [`Traditional] set.
      @param block_splitting_threshold The maximum number of consecutive, non-highlighted lines allowed in a block. The function will try to minimize the number of blocks, as long as no block has too many consecutive, non-highlighted lines. A higher threshold will lead to fewer blocks. When the threshold is zero, it means no block can contain any non-highlighted line. The default value is [5].
      @param tab_size The number of spaces that should be used to replace a horizontal tab. Note that a horizontal tab is always expanded to the same number of spaces. The result should still be visually appealing as long as horizontal tabs are only used at the beginning of lines. The default value is [8].
      @param debug Whether to enable the debug mode that performs expensive extra checking. The default is [false].

      @raise Invalid_argument if [use_color] is explicitly set to [true] but [use_ansi] is explicitly set to [false], or if [tab_size < 0], or if invalid ranges are detected. When the debug mode is enabled, detection of invalid ranges will raise the more structured exception {!exception:Source_marker.Invalid_range} instead.
      @raise Invalid_range if the debug mode is enabled and invalid ranges are detected. See {!exception:Source_marker.Invalid_range} for the detailed listing of all possible errors being reported.

      @see <https://no-color.org/> for the [NO_COLOR] specification
  *)
  val display : ?output:out_channel -> ?use_ansi:bool -> ?use_color:bool -> ?show_backtrace:bool -> ?marker:marker -> ?line_breaks:[`Unicode | `Traditional] -> ?block_splitting_threshold:int -> ?tab_size:int -> ?debug:bool -> Message.t Diagnostic.t -> unit
end
