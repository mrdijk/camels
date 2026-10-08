type t = TInt | TFloat | TStr | TBool

let of_value = function
  | Value.VInt _ -> TInt
  | Value.VFloat _ -> TFloat
  | Value.VStr _ -> TStr
  | Value.VBool _ -> TBool

let to_string = function
  | TInt -> "Int"
  | TFloat -> "Float"
  | TStr -> "Str"
  | TBool -> "Bool"

let widen a b =
  match a, b with
  | t1, t2 when t1 = t2 -> t1
  | TInt, TFloat | TFloat, TInt -> TFloat
  | _ -> TStr 
