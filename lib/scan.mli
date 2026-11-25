(** Reading a {!Source.t}. This is the only module that knows how each kind of
    source is read; the executor goes through {!iter_chunks} and never
    mentions a format. *)

(** [iter_chunks source ~columns f] calls [f] on the rows of [source], one
    frame per chunk (an in-memory source is a single chunk, a CSV file is
    streamed). Only [columns] are produced ([None] means all); a format may
    skip parsing the others entirely. The columns of each frame are in an
    unspecified order. [f] is called at least once, with an empty frame if
    there are no rows. *)
val iter_chunks : Source.t -> columns:string list option -> (Dataframe.t -> unit) -> unit
