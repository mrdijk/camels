type t = { plan : Plan.t }

let of_rows data ~columns = { plan = Plan.scan data ~columns }
let filter (df : t) (e : Expr.t) : t = { plan = Plan.filter df.plan e }
let schema (df : t) : Schema.t = Plan.schema df.plan
let explain (df : t) : string = Plan.explain df.plan

(* select "a" "b" -> project [("a", Col "a"); ("b", Col "b")] *)
let select (df : t) (columns : string list) : t =
  let exprs = List.map (fun name -> (name, Expr.col name)) columns in
  { plan = Plan.project df.plan exprs }

let with_column (df : t) (name : string) (e : Expr.t) : t =
  { plan = Plan.with_column df.plan name e }

let collect (df : t) : Row.t list =
  Plan.execute_batched df.plan |> Seq.concat_map List.to_seq |> List.of_seq
