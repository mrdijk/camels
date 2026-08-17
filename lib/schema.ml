type column = { name : string; dtype : Dtype.t; nullable : bool }
type t = column list

let make cols = cols
let column_names t = List.map (fun c -> c.name) t
let dtype_of t name = (List.find (fun c -> c.name = name) t).dtype
let nullable_of t name = (List.find (fun c -> c.name = name) t).nullable
let col_map t = List.mapi (fun i c -> (c.name, i)) t
let index_of t name = List.assoc name (col_map t)
let select t names = List.map (fun n -> List.find (fun c -> c.name = n) t) names
let with_column t name dtype ~nullable = t @ [ { name; dtype; nullable } ]
let force_nullable (t : t) : t = List.map (fun c ->  { c with nullable = true }) t

let merge (t : t) ?(suffix = "right_") (other : t) : t =
  let names = column_names t in
  let renamed =
    List.map
      (fun c -> if List.mem c.name names then { c with name = suffix ^ c.name } else c)
      other
  in
  t @ renamed

let infer_from_rows (names : string list) (rows : Row.t list) : t =
  let sample = List.filteri (fun i _ -> i < 1000) rows in
  List.mapi
    (fun i name ->
      let dtype = ref Dtype.TStr and seen_value = ref false and nullable = ref false in
      List.iter
        (fun (row : Row.t) ->
          match row.(i) with
          | None -> nullable := true
          | Some v ->
              if not !seen_value then begin
                dtype := Dtype.of_value v;
                seen_value := true
              end)
        sample;
      { name; dtype = !dtype; nullable = !nullable })
    names

let to_string (t : t) : string =
  t
  |> List.map (fun c ->
         Printf.sprintf "%s: %s%s" c.name (Dtype.to_string c.dtype) (if c.nullable then "?" else ""))
  |> String.concat ", "
  |> Printf.sprintf "Schema(%s)"
