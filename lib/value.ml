type t = VInt of int | VFloat of float | VStr of string | VBool of bool

let to_string = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VStr s -> Printf.sprintf "%S" s
  | VBool b -> string_of_bool b

let str s = VStr s
let int i = VInt i
let float f = VFloat f
let bool b = VBool b
