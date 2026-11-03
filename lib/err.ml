(** Single error type used across the library. *)
exception Polars_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Polars_error s)) fmt

