type value =
  | VInt of int
  | VStr of string
  | VFloat of float
  | VBool of bool

type binop =
  | Gt | Lt | Ge | Le | Eq | Ne
  | Add | Sub | Mul | Div

type unop = Neg

type expr =
  | Col of string
  | Lit of value
  | UnaryExpr of unop * expr
  | BinaryExpr of binop * expr * expr

type plan =
  | Scan of (string * value) list list
  | Filter of plan * expr

module StrSet = Set.Make (String)

let rec required_columns (e : expr) : StrSet.t =
  match e with
  | Col name -> StrSet.singleton name
  | Lit _ -> StrSet.empty 
  | UnaryExpr (_, e) -> required_columns e
  | BinaryExpr (_, l, r) ->
    StrSet.union (required_columns l) (required_columns r)

let apply_unop (op : unop) (v : value) : value =
  match (op, v) with
  | Neg, VInt i -> VInt (-i)
  | Neg, VFloat f -> VFloat (-.f)
  | Neg, (VStr _ | VBool _ ) -> failwith "cannot negate this value"

let apply_binop (op : binop) (a : value) (b : value) : value =
  match (op, a, b) with
  (* comparisons: int *)
  | Gt, VInt x, VInt y -> VBool (x > y)
  | Lt, VInt x, VInt y -> VBool (x < y)
  | Ge, VInt x, VInt y -> VBool (x >= y)
  | Le, VInt x, VInt y -> VBool (x <= y)
  | Eq, VInt x, VInt y -> VBool (x = y)
  | Ne, VInt x, VInt y -> VBool (x <> y)
  (* comparisons: float *)
  | Gt, VFloat x, VFloat y -> VBool (x > y)
  | Lt, VFloat x, VFloat y -> VBool (x < y)
  | Ge, VFloat x, VFloat y -> VBool (x >= y)
  | Le, VFloat x, VFloat y -> VBool (x <= y)
  | Eq, VFloat x, VFloat y -> VBool (Float.equal x y)
  | Ne, VFloat x, VFloat y -> VBool (not (Float.equal x y))
  (* comparisons: string *)
  | Gt, VStr x, VStr y -> VBool (String.compare x y > 0)
  | Lt, VStr x, VStr y -> VBool (String.compare x y < 0)
  | Ge, VStr x, VStr y -> VBool (String.compare x y >= 0)
  | Le, VStr x, VStr y -> VBool (String.compare x y <= 0)
  | Eq, VStr x, VStr y -> VBool (String.equal x y)
  | Ne, VStr x, VStr y -> VBool (not (String.equal x y))
  (* arithmetic: int *)
  | Add, VInt x, VInt y -> VInt (x + y)
  | Sub, VInt x, VInt y -> VInt (x - y)
  | Mul, VInt x, VInt y -> VInt (x * y)
  | Div, VInt x, VInt y -> VInt (x / y)
  (* arithmetic: float *)
  | Add, VFloat x, VFloat y -> VFloat (x +. y)
  | Sub, VFloat x, VFloat y -> VFloat (x -. y)
  | Mul, VFloat x, VFloat y -> VFloat (x *. y)
  | Div, VFloat x, VFloat y -> VFloat (x /. y)
  | _ -> failwith "type mismatch or unsupported operand types"

let rec eval (row : (string * value) list) (e : expr) :  value =
  match e with
  | Col name -> List.assoc name row
  | Lit v -> v
  | UnaryExpr (op, e) -> apply_unop op (eval row e)
  | BinaryExpr (op, l, r) -> apply_binop op (eval row l) (eval row r)

let eval_bool (row : (string * value) list) (e : expr) : bool =
  match eval row e with
  | VBool b -> b
  | _ -> failwith "expression did not evaluate to a boolean"

let filter_rows (rows : (string * value) list list) (e : expr) :
    (string * value) list list =
  List.filter (fun row -> eval_bool row e) rows

let rec execute (p : plan) : (string * value) list Seq.t =
  match p with
  | Scan data -> List.to_seq data
  | Filter (child, e) -> Seq.filter (fun row -> eval_bool row e) (execute child)
  
type operand =
  | E of expr
  | I of int
  | F of float
  | S of string
  | B of bool

let to_expr = function
  | E e -> e
  | I i -> Lit (VInt i)
  | F f -> Lit (VFloat f)
  | S s -> Lit (VStr s)
  | B b -> Lit (VBool b)

let ( ~- ) (e : expr) : expr = UnaryExpr (Neg, e)
let ( >. ) (l : expr) (r : operand) : expr = BinaryExpr (Gt, l, to_expr r)
let ( <. ) (l : expr) (r : operand) : expr = BinaryExpr (Lt, l, to_expr r)
let ( >=. ) (l : expr) (r : operand) : expr = BinaryExpr (Ge, l, to_expr r)
let ( <=. ) (l : expr) (r : operand) : expr = BinaryExpr (Le, l, to_expr r)
let ( =. ) (l : expr) (r : operand) : expr = BinaryExpr (Eq, l, to_expr r)
let ( +. ) (l : expr) (r : operand) : expr = BinaryExpr (Add, l, to_expr r)

let binop_to_string = function
  | Gt -> ">" | Lt -> "<" | Ge -> ">=" | Le -> "<="
  | Eq -> "=" | Ne -> "<>"
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let unop_to_string = function
  | Neg -> "-"

let value_to_string = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VStr s -> Printf.sprintf "%S" s   (* like Python's repr() for strings, adds quotes *)
  | VBool b -> string_of_bool b

let rec to_string (e : expr) : string =
  match e with
  | Col name -> Printf.sprintf "col(%S)" name
  | Lit v -> value_to_string v
  | UnaryExpr (op, e) -> Printf.sprintf "%s%s" (unop_to_string op) (to_string e)
  | BinaryExpr (op, l, r) ->
      Printf.sprintf "(%s %s %s)" (to_string l) (binop_to_string op) (to_string r)


let row_list = [ [("name", VStr "Alice"); ("age", VInt 30); ("region", VStr "EU")];
                 [("name", VStr "Bob");   ("age", VInt 25); ("region", VStr "US")]
               ]

let query = Col "age" >. I 28

let result = filter_rows row_list query
(* [ [("name", VStr "Alice"); ("age", VInt 30); ("region", VStr "EU")] ] *)

let scan = Scan row_list
let plan = Filter (scan, Col "age" >. I 28)
let result = List.of_seq (execute plan)

let () =
  List.iter
    (fun row ->
      List.iter
        (fun (k, v) -> Printf.printf "%s=%s " k (value_to_string v))
        row;
      print_newline ())
    result
(* name="Alice" age=30 region="EU" *)
