type t =
  | IntCol of int array
  | FloatCol of float array
  | StrCol of string array
  | BoolCol of bool array

let length = function
  | IntCol a -> Array.length a
  | FloatCol a -> Array.length a
  | StrCol a -> Array.length a
  | BoolCol a -> Array.length a

let get (s : t) (i : int) : Value.t =
  match s with
  | IntCol a -> Value.VInt a.(i)
  | FloatCol a -> Value.VFloat a.(i)
  | StrCol a -> Value.VStr a.(i)
  | BoolCol a -> Value.VBool a.(i)

let filter_array (a : 'a array) (mask : bool array) : 'a array =
  let acc = ref [] in
  Array.iteri (fun i keep -> if keep then acc := a.(i) :: !acc) mask;
  Array.of_list (List.rev !acc)

let apply_mask (s : t) (mask : bool array) : t =
  match s with
  | IntCol a -> IntCol (filter_array a mask)
  | FloatCol a -> FloatCol (filter_array a mask)
  | StrCol a -> StrCol (filter_array a mask)
  | BoolCol a -> BoolCol (filter_array a mask)

let of_values (vs : Value.t array) : t =
  if Array.length vs = 0 then BoolCol [||]
  else
    match vs.(0) with
    | Value.VInt _ -> IntCol (Array.map (function Value.VInt i -> i | _ -> failwith "mixed types") vs)
    | Value.VFloat _ -> FloatCol (Array.map (function Value.VFloat f -> f | _ -> failwith "mixed types") vs)
    | Value.VStr _ -> StrCol (Array.map (function Value.VStr s -> s | _ -> failwith "mixed types") vs)
    | Value.VBool _ -> BoolCol (Array.map (function Value.VBool b -> b | _ -> failwith "mixed types") vs)
