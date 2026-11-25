let default_chunk_bytes = 16 * 1024 * 1024

let read_csv_string ?(sep = ',') ?(quote = '"') ?(has_header = true) ?(null_values = [ "" ])
    ?(infer_rows = 100) ?(schema = []) text =
  Csv.of_string { Csv_options.sep; quote; has_header; null_values } ~infer_rows ~schema text

let read_csv ?(sep = ',') ?(quote = '"') ?(has_header = true) ?(null_values = [ "" ])
    ?(infer_rows = 100) ?(schema = []) ?(chunk_bytes = default_chunk_bytes) path =
  let o = { Csv_options.sep; quote; has_header; null_values } in
  let r = Csv.resolve_file o ~infer_rows ~schema ~chunk_bytes path in
  let chunks = ref [] in
  Csv.read_chunks o r ~chunk_bytes ~path ~columns:None (fun df -> chunks := df :: !chunks);
  Dataframe.concat (List.rev !chunks)

let scan_csv ?(sep = ',') ?(quote = '"') ?(has_header = true) ?(null_values = [ "" ])
    ?(infer_rows = 100) ?(schema = []) ?(chunk_bytes = default_chunk_bytes) path =
  let o = { Csv_options.sep; quote; has_header; null_values } in
  let r = Csv.resolve_file o ~infer_rows ~schema ~chunk_bytes path in
  Lazyframe.of_source
    (Source.Csv { path; options = o; schema = Csv.schema_of_resolved r; chunk_bytes })
