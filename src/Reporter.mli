(** The signature of a reporter.

    @inline *)
include module type of Reporter_sigs

(** The functor to generate a reporter. *)
module Make (Message : Message) : S with module Message := Message
