type binop = Gt | Lt | Ge | Le | Eq | Ne | Add | Sub | Mul | Div
type unop = Neg

type t =
  | Col of string
  | Lit of Value.t
  | UnaryExpr of unop * t
  | BinaryExpr of binop * t * t

module StrSet = Set.Make (String)

let rec required_columns = function
  | Col name -> StrSet.singleton name
  | Lit _ -> StrSet.empty
  | UnaryExpr (_, e) -> required_columns e
  | BinaryExpr (_, l, r) ->
      StrSet.union (required_columns l) (required_columns r)

let apply_binop op a b =
  match (op, a, b) with
  | Gt, Value.VInt x, Value.VInt y -> Value.VBool (x > y)
  | Lt, Value.VInt x, Value.VInt y -> Value.VBool (x < y)
  | Ge, Value.VInt x, Value.VInt y -> Value.VBool (x >= y)
  | Le, Value.VInt x, Value.VInt y -> Value.VBool (x <= y)
  | Eq, Value.VInt x, Value.VInt y -> Value.VBool (x = y)
  | Ne, Value.VInt x, Value.VInt y -> Value.VBool (x <> y)
  | Add, Value.VInt x, Value.VInt y -> Value.VInt (x + y)
  | Sub, Value.VInt x, Value.VInt y -> Value.VInt (x - y)
  | Mul, Value.VInt x, Value.VInt y -> Value.VInt (x * y)
  | Div, Value.VInt x, Value.VInt y -> Value.VInt (x / y)
  | Gt, Value.VStr x, Value.VStr y -> Value.VBool (String.compare x y > 0)
  | Eq, Value.VStr x, Value.VStr y -> Value.VBool (String.equal x y)
  | _ -> failwith "type mismatch or unsupported operand types"

let apply_unop op v =
  match (op, v) with
  | Neg, Value.VInt i -> Value.VInt (-i)
  | Neg, Value.VFloat f -> Value.VFloat (-.f)
  | Neg, _ -> failwith "cannot negate this value"

let rec eval (row : Row.t) (e : t) : Value.t =
  match e with
  | Col name -> Row.get row name
  | Lit v -> v
  | UnaryExpr (op, e) -> apply_unop op (eval row e)
  | BinaryExpr (op, l, r) -> apply_binop op (eval row l) (eval row r)

let eval_bool row e =
  match eval row e with
  | Value.VBool b -> b
  | _ -> failwith "expression did not evaluate to a boolean"

let col name = Col name
let lit v = Lit v

type operand = E of t | I of int | F of float | S of string | B of bool

let to_expr = function
  | E e -> e
  | I i -> Lit (Value.VInt i)
  | F f -> Lit (Value.VFloat f)
  | S s -> Lit (Value.VStr s)
  | B b -> Lit (Value.VBool b)

let ( >. ) l r = BinaryExpr (Gt, l, to_expr r)
let ( <. ) l r = BinaryExpr (Lt, l, to_expr r)
let ( =. ) l r = BinaryExpr (Eq, l, to_expr r)
let neg e = UnaryExpr (Neg, e)

let binop_to_string = function
  | Gt -> ">" | Lt -> "<" | Ge -> ">=" | Le -> "<="
  | Eq -> "=" | Ne -> "<>" | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec to_string = function
  | Col name -> Printf.sprintf "col(%S)" name
  | Lit v -> Value.to_string v
  | UnaryExpr (Neg, e) -> "-" ^ to_string e
  | BinaryExpr (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (to_string l) (binop_to_string op) (to_string r)

let apply_binop_arr (op : binop) (a : Series.t) (b : Series.t) : Series.t =
  match (op, a, b) with
  | Gt, Series.IntCol xs, Series.IntCol ys ->
      Series.BoolCol (Array.map2 (fun x y -> x > y) xs ys)
  | Lt, Series.IntCol xs, Series.IntCol ys ->
      Series.BoolCol (Array.map2 (fun x y -> x < y) xs ys)
  | Eq, Series.IntCol xs, Series.IntCol ys ->
      Series.BoolCol (Array.map2 (fun x y -> x = y) xs ys)
  | Add, Series.IntCol xs, Series.IntCol ys ->
      Series.IntCol (Array.map2 ( + ) xs ys)
  | Gt, Series.FloatCol xs, Series.FloatCol ys ->
      Series.BoolCol (Array.map2 (fun x y -> x > y) xs ys)
  | Gt, Series.StrCol xs, Series.StrCol ys ->
      Series.BoolCol (Array.map2 (fun x y -> String.compare x y > 0) xs ys)
  | _ -> failwith "unsupported or mismatched types in vectorized binop"

let broadcast (v : Value.t) (n : int) : Series.t =
  match v with
  | Value.VInt i -> Series.IntCol (Array.make n i)
  | Value.VFloat f -> Series.FloatCol (Array.make n f)
  | Value.VStr s -> Series.StrCol (Array.make n s)
  | Value.VBool b -> Series.BoolCol (Array.make n b)

let rec eval_batch (batch : Batch.t) (e : t) : Series.t =
  match e with
  | Col name -> Batch.get_column batch name
  | Lit v -> broadcast v (Batch.length batch)
  | UnaryExpr (Neg, e) -> (
      match eval_batch batch e with
      | Series.IntCol a -> Series.IntCol (Array.map (fun x -> -x) a)
      | Series.FloatCol a -> Series.FloatCol (Array.map (fun x -> -.x) a)
      | _ -> failwith "cannot negate this column")
  | BinaryExpr (op, l, r) -> apply_binop_arr op (eval_batch batch l) (eval_batch batch r)

let eval_mask (batch : Batch.t) (e : t) : bool array =
  match eval_batch batch e with
  | Series.BoolCol a -> a
  | _ -> failwith "expression did not evaluate to a boolean column"
