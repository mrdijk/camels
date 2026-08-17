type t = { plan : Plan.t }

let of_rows data ~columns = { plan = Plan.scan data ~columns }
let filter (df : t) (e : Expr.t) : t = { plan = Plan.filter df.plan e }
let select (df : t) (cols : string list) : t = { plan = Plan.select df.plan cols}
let schema (df : t) : Schema.t = Plan.schema df.plan
let explain (df : t) : string = Plan.explain df.plan

let collect (df : t) : Row.t list =
  Plan.execute_batched df.plan |> Seq.concat_map List.to_seq |> List.of_seq
