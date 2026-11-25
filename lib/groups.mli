(** Typed grouping and join-key machinery.

    Key columns are turned into dense integer codes (no boxed per-row keys),
    using a direct lookup table when the integer range is small and an
    open-addressing hash table otherwise. *)

type t = {
  codes : int array;  (** group id of each row; ids are in order of first appearance *)
  ngroups : int;
  first_rows : int array;  (** first row of each group ([-1] only for the key-less group of an empty frame) *)
  mutable rows : int array array option;  (** cache for {!rows} *)
}

(** [of_keys keys n] groups the [n] rows by the given key columns. Null is a
    value like any other (all nulls of a column fall in one group). With no
    keys there is a single group holding every row. *)
val of_keys : Series.t list -> int -> t

(** Number of rows in each group. *)
val sizes : t -> int array

(** Last row of each group. *)
val last_rows : t -> int array

(** Row indices of each group, in row order (built on first use). *)
val rows : t -> int array array

(** [join_codes left_keys right_keys] gives [(lcodes, rcodes, ncodes)]: rows
    with equal keys get equal codes. A row whose key has a null, and a left row
    whose key never occurs on the right, get [-1]. Int and float keys are
    compared as floats; other dtype mismatches raise {!Err.Polars_error}. *)
val join_codes : Series.t list -> Series.t list -> int array * int array * int
