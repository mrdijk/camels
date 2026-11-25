(** Fixed-length bit/byte mask. [true] = set. *)
type t

(** [create n v] is a mask of [n] bits, all equal to [v]. *)
val create : int -> bool -> t

(** Number of bits. *)
val length : t -> int

(** [get m i] reads bit [i] ([true] = set). *)
val get : t -> int -> bool

(** [set m i v] writes bit [i] in place. *)
val set : t -> int -> bool -> unit

(** Bitwise AND into a new mask; both masks must have the same length. *)
val and_ : t -> t -> t

(** Number of set bits. *)
val count_set : t -> int

(** [concat masks] joins masks end to end. *)
val concat : t list -> t

(** [sub m pos len] is a copy of [len] bits starting at [pos]. *)
val sub : t -> int -> int -> t

(** Adopts a byte buffer as a mask without copying: byte [i] is bit [i]
    ([0] = clear, anything else = set). The buffer must not be mutated
    afterwards by the caller. *)
val of_bytes : Bytes.t -> t
