let iter_chunks (src : Source.t) ~columns (f : Dataframe.t -> unit) =
  match src with
  | Source.Memory df ->
      f (match columns with None -> df | Some cols -> Dataframe.select_columns df cols)
  | Source.Csv c ->
      Csv.read_chunks c.options
        (Csv.resolved_of_schema c.schema)
        ~chunk_bytes:c.chunk_bytes ~path:c.path ~columns f
