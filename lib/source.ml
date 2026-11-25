type csv = {
  path : string;
  options : Csv_options.t;
  schema : (string * Dtype.t) list;
  chunk_bytes : int;
}

type t = Memory of Dataframe.t | Csv of csv

let schema = function
  | Memory df ->
      Array.to_list
        (Array.map (fun (s : Series.t) -> (s.name, Series.dtype s)) df.Dataframe.columns)
  | Csv c -> c.schema

let columns t = List.map fst (schema t)

let describe t =
  let names = String.concat ", " (columns t) in
  match t with
  | Memory _ -> Printf.sprintf "DF [%s]" names
  | Csv c -> Printf.sprintf "CSV SCAN %s [%s]" c.path names
