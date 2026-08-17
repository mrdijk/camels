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

let apply_binop (op : binop) (a : Value.t) (b : Value.t) : Value.t =
  match (op, a, b) with
  (* comparisons: int *)
  | Gt, Value.VInt x, Value.VInt y -> Value.VBool (x > y)
  | Lt, Value.VInt x, Value.VInt y -> Value.VBool (x < y)
  | Ge, Value.VInt x, Value.VInt y -> Value.VBool (x >= y)
  | Le, Value.VInt x, Value.VInt y -> Value.VBool (x <= y)
  | Eq, Value.VInt x, Value.VInt y -> Value.VBool (x = y)
  | Ne, Value.VInt x, Value.VInt y -> Value.VBool (x <> y)
  (* comparisons: float *)
  | Gt, Value.VFloat x, Value.VFloat y -> Value.VBool (x > y)
  | Lt, Value.VFloat x, Value.VFloat y -> Value.VBool (x < y)
  | Ge, Value.VFloat x, Value.VFloat y -> Value.VBool (x >= y)
  | Le, Value.VFloat x, Value.VFloat y -> Value.VBool (x <= y)
  | Eq, Value.VFloat x, Value.VFloat y -> Value.VBool (Float.equal x y)
  | Ne, Value.VFloat x, Value.VFloat y -> Value.VBool (not (Float.equal x y))
  (* comparisons: mixed int/float, promote to float *)
  | Gt, Value.VInt x, Value.VFloat y -> Value.VBool (float_of_int x > y)
  | Gt, Value.VFloat x, Value.VInt y -> Value.VBool (x > float_of_int y)
  | Lt, Value.VInt x, Value.VFloat y -> Value.VBool (float_of_int x < y)
  | Lt, Value.VFloat x, Value.VInt y -> Value.VBool (x < float_of_int y)
  (* comparisons: string *)
  | Gt, Value.VStr x, Value.VStr y -> Value.VBool (String.compare x y > 0)
  | Lt, Value.VStr x, Value.VStr y -> Value.VBool (String.compare x y < 0)
  | Ge, Value.VStr x, Value.VStr y -> Value.VBool (String.compare x y >= 0)
  | Le, Value.VStr x, Value.VStr y -> Value.VBool (String.compare x y <= 0)
  | Eq, Value.VStr x, Value.VStr y -> Value.VBool (String.equal x y)
  | Ne, Value.VStr x, Value.VStr y -> Value.VBool (not (String.equal x y))
  (* arithmetic: int *)
  | Add, Value.VInt x, Value.VInt y -> Value.VInt (x + y)
  | Sub, Value.VInt x, Value.VInt y -> Value.VInt (x - y)
  | Mul, Value.VInt x, Value.VInt y -> Value.VInt (x * y)
  | Div, Value.VInt x, Value.VInt y -> Value.VInt (x / y)
  (* arithmetic: float *)
  | Add, Value.VFloat x, Value.VFloat y -> Value.VFloat (x +. y)
  | Sub, Value.VFloat x, Value.VFloat y -> Value.VFloat (x -. y)
  | Mul, Value.VFloat x, Value.VFloat y -> Value.VFloat (x *. y)
  | Div, Value.VFloat x, Value.VFloat y -> Value.VFloat (x /. y)
  (* arithmetic: mixed int/float, promote to float *)
  | Add, Value.VInt x, Value.VFloat y -> Value.VFloat (float_of_int x +. y)
  | Add, Value.VFloat x, Value.VInt y -> Value.VFloat (x +. float_of_int y)
  | Sub, Value.VInt x, Value.VFloat y -> Value.VFloat (float_of_int x -. y)
  | Sub, Value.VFloat x, Value.VInt y -> Value.VFloat (x -. float_of_int y)
  | Mul, Value.VInt x, Value.VFloat y -> Value.VFloat (float_of_int x *. y)
  | Mul, Value.VFloat x, Value.VInt y -> Value.VFloat (x *. float_of_int y)
  | Div, Value.VInt x, Value.VFloat y -> Value.VFloat (float_of_int x /. y)
  | Div, Value.VFloat x, Value.VInt y -> Value.VFloat (x /. float_of_int y)
  | _ -> failwith "type mismatch or unsupported operand types"

let apply_unop op v =
  match (op, v) with
  | Neg, Value.VInt i -> Value.VInt (-i)
  | Neg, Value.VFloat f -> Value.VFloat (-.f)
  | Neg, _ -> failwith "cannot negate this value"

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


let output_name (e : t) : string option =
  match e with
    | Col name -> Some name
    | Lit _ | UnaryExpr _ | BinaryExpr _ -> None
  
let rec output_dtype (schema : Schema.t) (e : t) : Dtype.t =
  match e with
  | Col name -> Schema.dtype_of schema name
  | Lit v -> Dtype.of_value v
  | UnaryExpr (_, e) -> output_dtype schema e
  | BinaryExpr (op, l, r) -> (
      match op with
      | Gt | Lt | Ge | Le | Eq | Ne -> Dtype.TBool
      | Add | Sub | Mul | Div -> output_dtype schema l)

let rec compile (col_map : (string * int) list) (e : t) : Row.t -> Value.t =
  match e with
  | Col name ->
      let idx = List.assoc name col_map in
      fun row -> Row.get row idx
  | Lit v -> fun _ -> v
  | UnaryExpr (op, e) ->
      let f = compile col_map e in
      fun row -> apply_unop op (f row)
  | BinaryExpr (op, l, r) ->
      let fl = compile col_map l and fr = compile col_map r in
      fun row -> apply_binop op (fl row) (fr row)

let compile_bool (col_map : (string * int) list) (e : t) : Row.t -> bool =
  let f = compile col_map e in
  fun row -> match f row with Value.VBool b -> b | _ -> failwith "predicate did not evaluate to bool"

let ( >. ) (l : t) (r : operand) : t = BinaryExpr (Gt, l, to_expr r)
