type t = { plan : Plan.t }

let of_rows_chunked data ~chunk_size = { plan = Plan.scan data ~chunk_size }
let of_rows data = of_rows_chunked data ~chunk_size:1024
let filter (df : t) (e : Expr.t) : t = { plan = Plan.filter df.plan e }

let collect (df : t) : Row.t list =
  Plan.execute df.plan
  |> Seq.concat_map (fun batch -> List.to_seq (Batch.to_rows batch))
  |> List.of_seq
