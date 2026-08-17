let batch_size = 1024

type t =
  | Scan of { data : Row.t list; columns : string list; schema : Schema.t }
  | Filter of { child : t; predicate : Expr.t }
  | Select of { child : t; columns : string list }
  
let scan data ~columns : t = Scan { data; columns; schema = Schema.infer_from_rows columns data }
let filter child predicate : t = Filter { child; predicate }
let select child columns = Select { child; columns }

let rec schema (p : t) : Schema.t =
  match p with
  | Scan { schema; _ } -> schema
  | Filter { child; _ } -> schema child
  | Select { child; columns } -> Schema.select (schema child) columns

let batched (rows : Row.t list) : Row.t list Seq.t =
  let rec go rows () =
    match rows with
    | [] -> Seq.Nil
    | _ ->
        let rec take n acc = function
          | rest when n = 0 -> (List.rev acc, rest)
          | [] -> (List.rev acc, [])
          | x :: xs -> take (n - 1) (x :: acc) xs
        in
        let chunk, rest = take batch_size [] rows in
        Seq.Cons (chunk, go rest)
  in
  go rows

let rec execute_batched (p : t) : Row.t list Seq.t =
  match p with
  | Scan { data; _ } -> batched data
  | Filter { child; predicate } ->
      let col_map = Schema.col_map (schema child) in
      let pred_fn = Expr.compile_bool col_map predicate in
      execute_batched child
      |> Seq.map (List.filter pred_fn)
      |> Seq.filter (fun chunk -> chunk <> [])
  | Select { child; columns } ->
      let parent_schema = schema child in
      let indices = List.map (Schema.index_of parent_schema) columns in
      execute_batched child
      |> Seq.map (List.map (fun row -> Array.of_list (List.map (Array.get row) indices)))

let rec explain (p : t) : string =
  match p with
  | Scan { data; columns; _ } ->
      Printf.sprintf "Scan [%s] (%d rows)" (String.concat ", " columns) (List.length data)
  | Filter { child; predicate } ->
      Printf.sprintf "Filter [%s]\n  %s" (Expr.to_string predicate) (explain child)
  | Select { child; columns } ->
    Printf.sprintf "Select [%s]\n  %s" (String.concat ", " columns) (explain child)
