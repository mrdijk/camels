type column = { name : string; dtype : Dtype.t}
type t = column list

let make cols = cols
let column_names t = List.map (fun c -> c.name) t
let dtype_of t name = (List.find (fun c -> c.name = name) t).dtype
let col_map t = List.mapi (fun i c -> (c.name, i)) t
let index_of t name = List.assoc name (col_map t)
let select (t : t) (names : string list) : t =
  List.map (fun n -> List.find (fun c -> c.name = n) t) names

let infer_from_rows (names : string list) (rows : Row.t list) : t =
  match rows with
  | [] -> List.map (fun n -> { name = n; dtype = Dtype.TStr }) names
  | first :: _ ->
    List.mapi (fun i n -> {name = n; dtype = Dtype.of_value first.(i) }) names

let to_string (t : t) : string =
  t
  |> List.map (fun c -> Printf.sprintf "%s: %s" c.name (Dtype.to_string c.dtype))
  |> String.concat ", "
  |> Printf.sprintf "Schema(%s)"
