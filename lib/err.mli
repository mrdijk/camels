(** Single error type used across the library. *)

(** Raised for schema errors (unknown column, duplicate output names), type
    errors during evaluation, and malformed joins. *)
exception Camels_error of string

(** [fail fmt ...] formats a message and raises {!Camels_error}. *)
val fail : ('a, unit, string, 'b) format4 -> 'a
