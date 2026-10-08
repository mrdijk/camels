let infer_dtype (values : string list) : Dtype.t =
  let non_empty = List.filter (fun s -> s <> "") values in
  if non_empty = [] then Dtype.TStr
  else if List.for_all (fun s -> int_of_string_opt s <> None) non_empty then Dtype.TInt
  else if List.for_all (fun s -> float_of_string_opt s <> None) non_empty then Dtype.TFloat
  else if List.for_all (fun s -> s = "true" || s = "false") non_empty then Dtype.TBool
  else Dtype.TStr

let cast_value (dtype : Dtype.t) (raw : string) : Value.t option =
  if raw = "" then None
  else
    Some
      (match dtype with
       | Dtype.TInt -> Value.VInt (int_of_string raw)
       | Dtype.TFloat -> Value.VFloat (float_of_string raw)
       | Dtype.TBool -> Value.VBool (raw = "true")
       | Dtype.TStr -> Value.VStr raw)

  let read_csv_lazy ?(sample_size = 100) (path : string) : Plan.t =
    (* --- 1. open once, read header + a sample, then close --- *)
    let ic = open_in path in
    let csv_in = Csv.of_channel ~has_header:true ic in
    let columns = Csv.Rows.header csv_in in

    let rec take_sample n acc =
      if n = 0 then List.rev acc
      else
        match Csv.Rows.next csv_in with
        | row -> take_sample (n - 1) (List.map (fun c -> Csv.Row.find row c) columns :: acc)
        | exception End_of_file -> List.rev acc
    in
    let sample_rows = take_sample sample_size [] in
    close_in ic;  let dtypes_and_nullable =
    List.mapi
      (fun i _ ->
        let col_vals = List.map (fun row -> List.nth row i) sample_rows in
        let dtype = infer_dtype col_vals in
        let nullable = List.exists (fun v -> v = "") col_vals in
        (dtype, nullable))
      columns
    in
    let schema =
      Schema.make
        (List.map2
           (fun name (dtype, nullable) -> { Schema.name; dtype; nullable })
           columns dtypes_and_nullable)
    in
    let dtypes = List.map fst dtypes_and_nullable in
    (* --- 2. factory: fresh channel + fresh Seq.t on every call --- *)
    let factory () : Row.t Seq.t =
      let ic = open_in path in
      let csv_in = Csv.of_channel ~has_header:true ic in
      let rec next () =
        match Csv.next csv_in with
        | row ->
            let cells = List.map2 (fun dtype raw -> cast_value dtype raw) dtypes row in
            Seq.Cons (Array.of_list cells, next)
        | exception End_of_file ->
            close_in ic;
            Seq.Nil
        | exception e ->
            close_in ic;
            raise e
      in
      next
    in
    Plan.source ~columns ~schema ~label:"CSV" factory
