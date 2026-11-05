(** Single error type used across the library. *)

exception Polars_error of string
(** Raised for schema errors (unknown column, duplicate output names), type
    errors during evaluation, and malformed joins. *)

val fail : ('a, unit, string, 'b) format4 -> 'a
(** [fail fmt ...] formats a message and raises {!Polars_error}. *)
