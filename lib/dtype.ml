type t = Int64 | Float64 | Utf8 | Boolean

let to_string = function
  | Int64 -> "i64" | Float64 -> "f64" | Utf8 -> "str" | Boolean -> "bool"

let is_numeric = function Int64 | Float64 -> true | Utf8 | Boolean -> false

let supertype a b =
  match (a, b) with
  | x, y when x = y -> Some x
  | Int64, Float64 | Float64, Int64 -> Some Float64
  | _ -> None
