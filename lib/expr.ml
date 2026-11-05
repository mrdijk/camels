(** Expression AST and a small DSL for building expressions.

    Typical use:
    {[ Expr.(col "salary" * float 1.1 |> alias "raised") ]}
    [Expr.(...)] brings the infix operators ([+ - * / = <> < <= > >= && ||])
    into scope, redefined to build expressions instead of computing values. *)

type binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Lt_eq | Gt | Gt_eq | And | Or

type agg = Sum | Mean | Min | Max | Count | Len | N_unique | First | Last

type t =
  | Col of string
  | Lit of Value.t
  | Binary of t * binop * t
  | Not of t
  | Is_null of t
  | Alias of t * string
  | Agg of agg * t

(* ------------------------------------------------------------------ *)
(* Analysis helpers                                                    *)
(* ------------------------------------------------------------------ *)

let rec output_name = function
  | Col n -> n
  | Lit _ -> "literal"
  | Binary (l, _, _) -> output_name l
  | Not e | Is_null e -> output_name e
  | Alias (_, n) -> n
  | Agg (Len, _) -> "len"
  | Agg (_, e) -> output_name e

(** Column names referenced by the expression, in first-use order. *)
let columns e =
  let rec go acc = function
    | Col n -> if List.mem n acc then acc else n :: acc
    | Lit _ -> acc
    | Binary (l, _, r) -> go (go acc l) r
    | Not e | Is_null e | Alias (e, _) | Agg (_, e) -> go acc e
  in
  List.rev (go [] e)

let rec has_agg = function
  | Agg _ -> true
  | Col _ | Lit _ -> false
  | Binary (l, _, r) -> has_agg l || has_agg r
  | Not e | Is_null e | Alias (e, _) -> has_agg e

let rec split_and = function
  | Binary (l, And, r) -> split_and l @ split_and r
  | e -> [ e ]

let and_all = function
  | [] -> None
  | p :: ps -> Some (List.fold_left (fun a b -> Binary (a, And, b)) p ps)

let binop_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Lt_eq -> "<="
  | Gt -> ">" | Gt_eq -> ">=" | And -> "&" | Or -> "|"

let agg_to_string = function
  | Sum -> "sum" | Mean -> "mean" | Min -> "min" | Max -> "max"
  | Count -> "count" | Len -> "len" | N_unique -> "n_unique"
  | First -> "first" | Last -> "last"

let rec to_string = function
  | Col n -> Printf.sprintf "col(%S)" n
  | Lit (Value.Str s) -> Printf.sprintf "%S" s
  | Lit v -> Value.to_string v
  | Binary (l, op, r) ->
      Printf.sprintf "(%s %s %s)" (to_string l) (binop_to_string op) (to_string r)
  | Not e -> "~" ^ to_string e
  | Is_null e -> to_string e ^ ".is_null()"
  | Alias (e, n) -> Printf.sprintf "%s.alias(%S)" (to_string e) n
  | Agg (Len, _) -> "len()"
  | Agg (a, e) -> Printf.sprintf "%s.%s()" (to_string e) (agg_to_string a)

(* ------------------------------------------------------------------ *)
(* DSL (kept at the end: it shadows Stdlib operators)                  *)
(* ------------------------------------------------------------------ *)

let col n = Col n
let int i = Lit (Value.Int i)
let float f = Lit (Value.Float f)
let str s = Lit (Value.Str s)
let bool b = Lit (Value.Bool b)
let null = Lit Value.Null

let alias name e = Alias (e, name)
let not_ e = Not e
let is_null e = Is_null e
let is_not_null e = Not (Is_null e)

let sum e = Agg (Sum, e)
let mean e = Agg (Mean, e)
let min e = Agg (Min, e)
let max e = Agg (Max, e)
let count e = Agg (Count, e) (* non-null values *)
let n_unique e = Agg (N_unique, e)
let first e = Agg (First, e)
let last e = Agg (Last, e)
let len () = Agg (Len, Lit (Value.Int 0)) (* number of rows *)

let ( + ) a b = Binary (a, Add, b)
let ( - ) a b = Binary (a, Sub, b)
let ( * ) a b = Binary (a, Mul, b)
let ( / ) a b = Binary (a, Div, b)
let ( = ) a b = Binary (a, Eq, b)
let ( <> ) a b = Binary (a, Neq, b)
let ( < ) a b = Binary (a, Lt, b)
let ( <= ) a b = Binary (a, Lt_eq, b)
let ( > ) a b = Binary (a, Gt, b)
let ( >= ) a b = Binary (a, Gt_eq, b)
let ( && ) a b = Binary (a, And, b)
let ( || ) a b = Binary (a, Or, b)
