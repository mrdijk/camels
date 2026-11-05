(** Expression evaluation over typed columns.

    An expression evaluates to a [Scalar] (literals, full-frame aggregates) or
    a [Column]. Binary operations dispatch on the operand dtypes once per
    column and then run a monomorphic loop over the buffers; validity masks
    are combined separately. Scalars are broadcast to constant columns when
    they meet a column. *)

module S = Series

type ev = Scalar of Value.t | Column of S.t

type ctx = {
  col : string -> ev;
  agg : Expr.agg -> Expr.t -> ev;
}

(* ------------------------------------------------------------------ *)
(* Scalar semantics (scalar op scalar)                                  *)
(* ------------------------------------------------------------------ *)

let type_error what a b =
  Err.fail "type error: cannot apply %s to %s and %s" what (Value.to_string a)
    (Value.to_string b)

let arith (op : Expr.binop) a b =
  let open Value in
  match (a, b) with
  | Null, _ | _, Null -> Null
  | Int x, Int y -> (
      match op with
      | Add -> Int (x + y)
      | Sub -> Int (x - y)
      | Mul -> Int (x * y)
      | Div -> Float (float_of_int x /. float_of_int y)
      | _ -> assert false)
  | (Int _ | Float _), (Int _ | Float _) -> (
      let f = function Int i -> float_of_int i | Float f -> f | _ -> assert false in
      let x = f a and y = f b in
      match op with
      | Add -> Float (x +. y)
      | Sub -> Float (x -. y)
      | Mul -> Float (x *. y)
      | Div -> Float (x /. y)
      | _ -> assert false)
  | Str x, Str y when op = Add -> Str (x ^ y)
  | _ -> type_error (Expr.binop_to_string op) a b

let comparable a b =
  let open Value in
  match (a, b) with
  | (Int _ | Float _), (Int _ | Float _) | Str _, Str _ | Bool _, Bool _ -> true
  | _ -> false

let test_of_op (op : Expr.binop) : int -> bool =
  match op with
  | Eq -> fun c -> c = 0
  | Neq -> fun c -> c <> 0
  | Lt -> fun c -> c < 0
  | Lt_eq -> fun c -> c <= 0
  | Gt -> fun c -> c > 0
  | Gt_eq -> fun c -> c >= 0
  | _ -> assert false

let compare_op (op : Expr.binop) a b =
  match (a, b) with
  | Value.Null, _ | _, Value.Null -> Value.Null
  | _ ->
      if not (comparable a b) then type_error (Expr.binop_to_string op) a b;
      Value.Bool (test_of_op op (Value.compare a b))

(* Kleene three-valued logic *)
let logic (op : Expr.binop) a b =
  let open Value in
  match (op, a, b) with
  | And, Bool false, _ | And, _, Bool false -> Bool false
  | And, Bool true, Bool true -> Bool true
  | Or, Bool true, _ | Or, _, Bool true -> Bool true
  | Or, Bool false, Bool false -> Bool false
  | (And | Or), (Null | Bool _), (Null | Bool _) -> Null
  | _ -> type_error (Expr.binop_to_string op) a b

let apply_binop (op : Expr.binop) a b =
  match op with
  | Add | Sub | Mul | Div -> arith op a b
  | Eq | Neq | Lt | Lt_eq | Gt | Gt_eq -> compare_op op a b
  | And | Or -> logic op a b

let not3 = function
  | Value.Bool b -> Value.Bool (not b)
  | Value.Null -> Value.Null
  | v -> Err.fail "type error: cannot negate %s" (Value.to_string v)

(* ------------------------------------------------------------------ *)
(* Column kernels                                                       *)
(* ------------------------------------------------------------------ *)

let and_validity (a : S.t) (b : S.t) =
  match (a.validity, b.validity) with
  | None, v | v, None -> v
  | Some x, Some y -> Some (Bitmap.and_ x y)

let floats_of (s : S.t) : float array =
  match s.data with
  | S.Floats a -> a
  | S.Ints a -> Array.map float_of_int a
  | _ -> assert false

let col_type_error what (a : S.t) (b : S.t) =
  Err.fail "type error: cannot apply %s to %s and %s columns" what
    (Dtype.to_string (S.dtype a)) (Dtype.to_string (S.dtype b))

(* Kernels compute straight through null slots (their data is a placeholder)
   and only combine the validity masks, so no per-element null branch. *)
let arith_kernel (op : Expr.binop) (a : S.t) (b : S.t) : S.t =
  let data : S.buffer =
    match (a.data, b.data) with
    | S.Ints x, S.Ints y -> (
        match op with
        | Add -> S.Ints (Array.map2 ( + ) x y)
        | Sub -> S.Ints (Array.map2 ( - ) x y)
        | Mul -> S.Ints (Array.map2 ( * ) x y)
        | Div -> S.Floats (Array.map2 (fun p q -> float_of_int p /. float_of_int q) x y)
        | _ -> assert false)
    | (S.Ints _ | S.Floats _), (S.Ints _ | S.Floats _) ->
        let f =
          match op with
          | Add -> ( +. ) | Sub -> ( -. ) | Mul -> ( *. ) | Div -> ( /. )
          | _ -> assert false
        in
        S.Floats (Array.map2 f (floats_of a) (floats_of b))
    | S.Strings x, S.Strings y when op = Add -> S.Strings (Array.map2 ( ^ ) x y)
    | _ -> col_type_error (Expr.binop_to_string op) a b
  in
  S.make "" data (and_validity a b)

let compare_kernel (op : Expr.binop) (a : S.t) (b : S.t) : S.t =
  let n = S.length a in
  let test = test_of_op op in
  let cmp : int -> int =
    match (a.data, b.data) with
    | S.Ints x, S.Ints y -> fun i -> Int.compare x.(i) y.(i)
    | (S.Ints _ | S.Floats _), (S.Ints _ | S.Floats _) ->
        let x = floats_of a and y = floats_of b in
        fun i -> Float.compare x.(i) y.(i)
    | S.Strings x, S.Strings y -> fun i -> String.compare x.(i) y.(i)
    | S.Bools x, S.Bools y -> fun i -> Bool.compare (Bitmap.get x i) (Bitmap.get y i)
    | _ -> col_type_error (Expr.binop_to_string op) a b
  in
  let out = Bitmap.create n false in
  for i = 0 to n - 1 do
    if test (cmp i) then Bitmap.set out i true
  done;
  S.make "" (S.Bools out) (and_validity a b)

let logic_kernel (op : Expr.binop) (a : S.t) (b : S.t) : S.t =
  match (a.data, b.data) with
  | S.Bools x, S.Bools y ->
      let n = S.length a in
      let out = Bitmap.create n false and valid = Bitmap.create n true in
      let has_null = ref false in
      for i = 0 to n - 1 do
        let av = S.is_valid a i and bv = S.is_valid b i in
        let ab = Bitmap.get x i and bb = Bitmap.get y i in
        (* "dominant" value: false for And, true for Or *)
        let dom = op = Expr.Or in
        if (av && ab = dom) || (bv && bb = dom) then (if dom then Bitmap.set out i true)
        else if av && bv then (if not dom then Bitmap.set out i true)
        else begin
          Bitmap.set valid i false;
          has_null := true
        end
      done;
      S.make "" (S.Bools out) (if !has_null then Some valid else None)
  | _ -> col_type_error (Expr.binop_to_string op) a b

let kernel (op : Expr.binop) a b =
  match op with
  | Add | Sub | Mul | Div -> arith_kernel op a b
  | Eq | Neq | Lt | Lt_eq | Gt | Gt_eq -> compare_kernel op a b
  | And | Or -> logic_kernel op a b

let not_kernel (s : S.t) : S.t =
  match s.data with
  | S.Bools m ->
      let n = S.length s in
      let out = Bitmap.create n false in
      for i = 0 to n - 1 do
        if not (Bitmap.get m i) then Bitmap.set out i true
      done;
      S.make "" (S.Bools out) s.validity
  | _ -> Err.fail "type error: cannot negate a %s column" (Dtype.to_string (S.dtype s))

let is_null_kernel (s : S.t) : S.t =
  let n = S.length s in
  let out = Bitmap.create n false in
  (match s.validity with
  | None -> ()
  | Some v ->
      for i = 0 to n - 1 do
        if not (Bitmap.get v i) then Bitmap.set out i true
      done);
  S.make "" (S.Bools out) None

(* ------------------------------------------------------------------ *)
(* Aggregations                                                         *)
(* ------------------------------------------------------------------ *)

(** Row-by-row comparison for the column's buffer type. *)
let cmp_rows (s : S.t) : int -> int -> int =
  match s.data with
  | S.Ints a -> fun i j -> Int.compare a.(i) a.(j)
  | S.Floats a -> fun i j -> Float.compare a.(i) a.(j)
  | S.Strings a -> fun i j -> String.compare a.(i) a.(j)
  | S.Bools m -> fun i j -> Bool.compare (Bitmap.get m i) (Bitmap.get m j)

(** Aggregate the [n] rows [at 0 .. at (n-1)] of [s]. *)
let agg_rows (agg : Expr.agg) (s : S.t) (n : int) (at : int -> int) : Value.t =
  let valid = S.is_valid s in
  let best ~better =
    let best = ref (-1) in
    let cmp = cmp_rows s in
    for k = 0 to n - 1 do
      let i = at k in
      if valid i && (!best < 0 || better (cmp i !best)) then best := i
    done;
    if !best < 0 then Value.Null else S.get s !best
  in
  match agg with
  | Len -> Int n
  | Count ->
      let c = ref 0 in
      for k = 0 to n - 1 do if valid (at k) then incr c done;
      Int !c
  | Sum -> (
      match s.data with
      | S.Ints a ->
          let acc = ref 0 in
          for k = 0 to n - 1 do let i = at k in if valid i then acc := !acc + a.(i) done;
          Int !acc
      | S.Floats a ->
          let acc = ref 0.0 in
          for k = 0 to n - 1 do let i = at k in if valid i then acc := !acc +. a.(i) done;
          Float !acc
      | _ -> Err.fail "cannot sum a %s column" (Dtype.to_string (S.dtype s)))
  | Mean -> (
      let acc = ref 0.0 and c = ref 0 in
      (match s.data with
      | S.Ints a ->
          for k = 0 to n - 1 do
            let i = at k in
            if valid i then (acc := !acc +. float_of_int a.(i); incr c)
          done
      | S.Floats a ->
          for k = 0 to n - 1 do
            let i = at k in
            if valid i then (acc := !acc +. a.(i); incr c)
          done
      | _ -> Err.fail "cannot take mean of a %s column" (Dtype.to_string (S.dtype s)));
      if !c = 0 then Null else Float (!acc /. float_of_int !c))
  | Min -> best ~better:(fun c -> c < 0)
  | Max -> best ~better:(fun c -> c > 0)
  | N_unique ->
      let tbl = Hashtbl.create 16 in
      for k = 0 to n - 1 do
        Hashtbl.replace tbl (Value.normalize (S.get s (at k))) ()
      done;
      Int (Hashtbl.length tbl)
  | First -> if n = 0 then Null else S.get s (at 0)
  | Last -> if n = 0 then Null else S.get s (at (n - 1))

(** Output dtype of an aggregation over a column of dtype [d]. *)
let agg_dtype (agg : Expr.agg) (d : Dtype.t) : Dtype.t =
  match (agg, d) with
  | (Len | Count | N_unique), _ -> Int64
  | Sum, (Int64 | Float64) -> d
  | Mean, (Int64 | Float64) -> Float64
  | (Sum | Mean), _ ->
      Err.fail "cannot apply %s to a %s column" (Expr.agg_to_string agg) (Dtype.to_string d)
  | (Min | Max | First | Last), _ -> d

let reduce (agg : Expr.agg) (s : S.t) : Value.t = agg_rows agg s (S.length s) Fun.id

let reduce_groups (agg : Expr.agg) (s : S.t) (groups : int array array) : S.t =
  let vals =
    Array.map (fun idx -> agg_rows agg s (Array.length idx) (Array.get idx)) groups
  in
  S.of_values ~dtype:(agg_dtype agg (S.dtype s)) "" vals

(* ------------------------------------------------------------------ *)
(* Evaluation                                                           *)
(* ------------------------------------------------------------------ *)

let broadcast ?like (v : Value.t) n : S.t =
  S.of_scalar ?dtype:(Option.map S.dtype like) "" n v

let binary op l r =
  match (l, r) with
  | Scalar x, Scalar y -> Scalar (apply_binop op x y)
  | Column x, Scalar y -> Column (kernel op x (broadcast ~like:x y (S.length x)))
  | Scalar x, Column y -> Column (kernel op (broadcast ~like:y x (S.length y)) y)
  | Column x, Column y ->
      if S.length x <> S.length y then
        Err.fail "length mismatch: %d vs %d" (S.length x) (S.length y);
      Column (kernel op x y)

let rec eval ctx (e : Expr.t) : ev =
  match e with
  | Col n -> ctx.col n
  | Lit v -> Scalar v
  | Binary (l, op, r) -> binary op (eval ctx l) (eval ctx r)
  | Not e -> (
      match eval ctx e with Scalar v -> Scalar (not3 v) | Column s -> Column (not_kernel s))
  | Is_null e -> (
      match eval ctx e with
      | Scalar v -> Scalar (Value.Bool (Value.is_null v))
      | Column s -> Column (is_null_kernel s))
  | Alias (e, _) -> eval ctx e
  | Agg (a, e) -> ctx.agg a e

let materialize ~height = function
  | Scalar v -> broadcast v height
  | Column s -> s

let frame_ctx (df : Dataframe.t) : ctx =
  let height = Dataframe.height df in
  let rec ctx =
    {
      col = (fun n -> Column (Dataframe.get_column df n));
      agg =
        (fun a e ->
          match a with
          | Len -> Scalar (Value.Int height)
          | _ -> Scalar (reduce a (materialize ~height (eval ctx e))));
    }
  in
  ctx

let group_ctx (df : Dataframe.t) (groups : int array array)
    (keys : (string * S.t) list) : ctx =
  let height = Dataframe.height df in
  let fctx = frame_ctx df in
  {
    col =
      (fun n ->
        match List.assoc_opt n keys with
        | Some s -> Column s
        | None ->
            Err.fail
              "column %S must be used inside an aggregation (it is not a group key)" n);
    agg =
      (fun a e ->
        match a with
        | Len ->
            Column (S.of_ints "" (Array.map Array.length groups))
        | _ -> Column (reduce_groups a (materialize ~height (eval fctx e)) groups));
  }
