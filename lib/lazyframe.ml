type t = { plan : Plan.t }

let of_rows data ~columns = { plan = Plan.scan data ~columns }

let filter (df : t) (e : Expr.t) : t = { plan = Plan.filter df.plan e }

let select (df : t) (columns : string list) : t =
  let exprs = List.map (fun name -> (name, Expr.col name)) columns in
  { plan = Plan.project df.plan exprs }

let with_column (df : t) (name : string) (e : Expr.t) : t =
  { plan = Plan.with_column df.plan name e }

let join (left : t) (right : t) ~left_on ~right_on ~how : t =
  { plan = Plan.join left.plan right.plan ~left_on ~right_on ~how }

type grouped = { gdf : t; group_cols : string list }

let group_by (df : t) (cols : string list) : grouped = { gdf = df; group_cols = cols }

let agg (g : grouped) (aggs : Agg.t list) : t =
  { plan = Plan.aggregate g.gdf.plan g.group_cols aggs }

let schema (df : t) : Schema.t = Plan.schema df.plan
let explain ?(optimize = false) (df : t) : string =
  let plan = if optimize then Plan.optimize df.plan else df.plan in
  Plan.explain plan

let collect ?(optimize = true) (df : t) : Row.t list =
  let plan = if optimize then Plan.optimize df.plan else df.plan in
  Plan.execute_batched plan |> Seq.concat_map List.to_seq |> List.of_seq

let to_rows (t : Csv_reader.t) : Row.t list * string list =
  let columns = t.column_names in
  let cols_arr = Array.of_list
    (List.map (fun name -> (Hashtbl.find t.columns name).values) columns) in
  let rows = List.init t.n_rows (fun i ->
    (Array.map (fun col_values -> col_values.(i)) cols_arr : Row.t)) in
  (rows, columns)

(* let read_csv_lazy ?has_header ?dtypes ?na_values (path : string) : t = *)
  (* let table = Csv_reader.read_csv ?has_header ?dtypes ?na_values path in *)
  (* let rows, columns = to_rows table in *)
  (* of_rows rows ~columns *)
let read_csv_lazy ?sample_size path : t = { plan = Io.read_csv_lazy ?sample_size path }
