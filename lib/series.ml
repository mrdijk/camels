type buffer =
  | Ints of int array
  | Floats of float array
  | Strings of string array
  | Bools of bool array

type t = {
  length : int;
  validity : bool array;
  data : buffer;
}

let length (s : t) : int = s.length

let get s i = if not s.validity.(i) then None
  else
  match s.data with
    | Ints arr -> Some (Value.VInt arr.(i))
    | Floats arr -> Some (Value.VFloat arr.(i))
    | Strings arr -> Some (Value.VStr arr.(i))
    | Bools arr ->Some (Value.VBool arr.(i))

let dtype (s : t) : Dtype.t =
  match s.data with
  | Ints _ -> Dtype.TInt
  | Floats _ -> Dtype.TFloat
  | Strings _ -> Dtype.TStr
  | Bools _ -> Dtype.TBool

let infer_dtype (values : Value.t option array) : Dtype.t =
  let rec find i =
    if i >= Array.length values then Dtype.TStr
    else
      match values.(i) with
      | Some v -> Dtype.of_value v
      | None -> find (i + 1)
  in
  find 0

let ints_of (values : Value.t option array) : int array =
  Array.mapi (fun idx v -> match v with
    | Some (Value.VInt i) -> i
    | None -> 0 (* placeholder for null *)
    | Some _ -> failwith (Printf.sprintf "VInt expected at index %d" idx))
    values

let floats_of (values : Value.t option array) : float array =
  Array.mapi (fun idx v -> match v with
    | Some (Value.VFloat f) -> f
    | None -> 0.0 (* placefolder for null *)
    | Some _ -> failwith (Printf.sprintf "VFloat expected at index %d" idx))
    values

  let strings_of (values : Value.t option array) : string array =
    Array.mapi (fun idx v -> match v with
    | Some (Value.VStr s) -> s
    | None -> ""
    | Some _ -> failwith (Printf.sprintf "VStr expected at index %d" idx))
    values

  let bools_of (values : Value.t option array) : bool array =
    Array.mapi (fun idx v -> match v with
    | Some (Value.VBool b) -> b
    | None -> false
    | Some _ -> failwith (Printf.sprintf "VBool expected at index %d" idx))
    values

let of_values (values : Value.t option array) : t =
  let length = Array.length values in
  let validity = Array.map (fun x ->
    match x with
    | None -> false
    | Some _ -> true
    ) values in
  let dtype = infer_dtype values in
  let data =
    match dtype with
    | Dtype.TInt -> Ints (ints_of values)
    | Dtype.TFloat -> Floats (floats_of values)
    | Dtype.TStr -> Strings (strings_of values)
    | Dtype.TBool -> Bools (bools_of values)
  in
  { length; validity; data }

let to_values (s : t) : Value.t option array =
  match s.data with
  | Ints v    -> Array.mapi (fun i x -> if s.validity.(i) then Some (Value.VInt x)   else None) v
  | Floats v  -> Array.mapi (fun i x -> if s.validity.(i) then Some (Value.VFloat x) else None) v
  | Strings v -> Array.mapi (fun i x -> if s.validity.(i) then Some (Value.VStr x)   else None) v
  | Bools v   -> Array.mapi (fun i x -> if s.validity.(i) then Some (Value.VBool x)  else None) v

