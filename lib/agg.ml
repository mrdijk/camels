type kind = Sum | Count | Mean | Min | Max

type t = { expr : Expr.t; kind : kind; alias : string option }

let sum expr = { expr; kind = Sum; alias = None }
let count expr = { expr; kind = Count; alias = None }
let mean expr = { expr; kind = Mean; alias = None }
let min expr = { expr; kind = Min; alias = None }
let max expr = { expr; kind = Max; alias = None }
let alias name t = { t with alias = Some name }

let kind_name = function
  | Sum -> "sum" | Count -> "count" | Mean -> "mean" | Min -> "min" | Max -> "max"

let output_name (t : t) : string =
  match t.alias with
  | Some a -> a
  | None ->
      let base = Option.value (Expr.output_name t.expr) ~default:"value" in
      Printf.sprintf "%s_%s" base (kind_name t.kind)

let output_dtype (schema : Schema.t) (t : t) : Dtype.t =
  match t.kind with
  | Count -> Dtype.TInt
  | Mean -> Dtype.TFloat
  | Sum | Min | Max -> Expr.output_dtype schema t.expr

let to_string (t : t) : string =
  let base = Printf.sprintf "%s.%s()" (Expr.to_string t.expr) (kind_name t.kind) in
  match t.alias with Some a -> Printf.sprintf "%s.alias(%S)" base a | None -> base

type acc =
  | SumAcc of { mutable s : Value.t option }   (* None until first value seen *)
  | CountAcc of { mutable n : int }
  | MeanAcc of { mutable s : float; mutable n : int }
  | MinAcc of { mutable v : Value.t option }
  | MaxAcc of { mutable v : Value.t option }

let init_acc (t : t) : acc =
  match t.kind with
  | Sum -> SumAcc { s = None }
  | Count -> CountAcc { n = 0 }
  | Mean -> MeanAcc { s = 0.0; n = 0 }
  | Min -> MinAcc { v = None }
  | Max -> MaxAcc { v = None }

let value_as_float (v : Value.t) : float =
  match v with
  | Value.VInt i -> float_of_int i
  | Value.VFloat f -> f
  | _ -> failwith "cannot aggregate a non-numeric value"

let value_add (a : Value.t) (b : Value.t) : Value.t =
  match (a, b) with
  | Value.VInt x, Value.VInt y -> Value.VInt (x + y)
  | _ -> Value.VFloat (value_as_float a +. value_as_float b)

let value_lt (a : Value.t) (b : Value.t) : bool =
  match (a, b) with
  | Value.VInt x, Value.VInt y -> x < y
  | Value.VStr x, Value.VStr y -> String.compare x y < 0
  | _ -> value_as_float a < value_as_float b

let update_acc (t : t) (acc : acc) (v : Value.t option) : unit =
  match (t.kind, acc, v) with
  | Sum, SumAcc r, Some v -> r.s <- Some (match r.s with None -> v | Some s -> value_add s v)
  | Sum, SumAcc _, None -> ()
  | Count, CountAcc r, Some _ -> r.n <- r.n + 1
  | Count, CountAcc _, None -> ()
  | Mean, MeanAcc r, Some v -> r.s <- r.s +. value_as_float v; r.n <- r.n + 1
  | Mean, MeanAcc _, None -> ()
  | Min, MinAcc r, Some v ->
      r.v <- Some (match r.v with None -> v | Some cur -> if value_lt v cur then v else cur)
  | Min, MinAcc _, None -> ()
  | Max, MaxAcc r, Some v ->
      r.v <- Some (match r.v with None -> v | Some cur -> if value_lt cur v then v else cur)
  | Max, MaxAcc _, None -> ()
  | _ -> failwith "accumulator/kind mismatch (internal error)"

let finalize_acc (t : t) (acc : acc) : Value.t option =
  match (t.kind, acc) with
  | Sum, SumAcc r -> ( match r.s with Some v -> Some v | None -> Some (Value.VInt 0))
  | Count, CountAcc r -> Some (Value.VInt r.n)
  | Mean, MeanAcc r -> Some (Value.VFloat (if r.n = 0 then 0.0 else r.s /. float_of_int r.n))
  | Min, MinAcc r -> r.v
  | Max, MaxAcc r -> r.v
  | _ -> failwith "accumulator/kind mismatch (internal error)" 
