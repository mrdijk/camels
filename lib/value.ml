(** Dynamically typed scalar values. [Null] is a member of every type. *)

type t = Null | Int of int | Float of float | Str of string | Bool of bool

type dtype = Int64 | Float64 | Utf8 | Boolean | Unknown

let dtype_to_string = function
  | Int64 -> "i64"
  | Float64 -> "f64"
  | Utf8 -> "str"
  | Boolean -> "bool"
  | Unknown -> "null"

let is_null = function Null -> true | _ -> false

let to_string = function
  | Null -> "null"
  | Int i -> string_of_int i
  | Float f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.1f" f
      else Printf.sprintf "%.10g" f
  | Str s -> s
  | Bool b -> string_of_bool b

let rank = function Null -> 0 | Bool _ -> 1 | Int _ | Float _ -> 2 | Str _ -> 3

(** Total order: nulls first, numbers compared numerically across Int/Float. *)
let compare a b =
  match (a, b) with
  | Null, Null -> 0
  | Int x, Int y -> Int.compare x y
  | Int x, Float y -> Float.compare (float_of_int x) y
  | Float x, Int y -> Float.compare x (float_of_int y)
  | Float x, Float y -> Float.compare x y
  | Str x, Str y -> String.compare x y
  | Bool x, Bool y -> Bool.compare x y
  | _ -> Int.compare (rank a) (rank b)

let equal a b = compare a b = 0

(** Canonical form used for hashing (so that [Int 1] and [Float 1.] collide). *)
let normalize = function
  | Float f when Float.is_integer f && Float.abs f < 4e18 -> Int (int_of_float f)
  | v -> v

let dtype_of = function
  | Null -> Unknown
  | Int _ -> Int64
  | Float _ -> Float64
  | Str _ -> Utf8
  | Bool _ -> Boolean
