(* ---------- Low-level tokenizer: string -> string list list ---------- *)

type state =
  | Start_field
  | In_field
  | In_quoted_field
  | Quote_in_quoted_field

let parse_string (s : string) : string list list =
  let len = String.length s in
  let rows = ref [] in
  let current_row = ref [] in
  let buf = Buffer.create 64 in
  let state = ref Start_field in

  let end_field () =
    current_row := Buffer.contents buf :: !current_row;
    Buffer.clear buf;
  in
  let end_row () =
    end_field ();
    rows := List.rev !current_row :: !rows;
    current_row := []
  in

  let i = ref 0 in
    while !i < len do
      let c = s.[!i] in
      (match !state, c with
       | Start_field, '"' -> state := In_quoted_field
       | Start_field, ',' -> end_field ()
       | Start_field, '\r' -> ()
       | Start_field, '\n' -> end_row ()
       | Start_field, _ -> Buffer.add_char buf c; state := In_field

       | In_field, ',' -> end_field (); state := Start_field
       | In_field, '\r' -> ()
       | In_field, '\n' -> end_row (); state := Start_field
       | In_field, _ -> Buffer.add_char buf c

       | In_quoted_field, '"' -> state := Quote_in_quoted_field
       | In_quoted_field, _ -> Buffer.add_char buf c

       | Quote_in_quoted_field, '"' ->
         Buffer.add_char buf '"'; state := In_quoted_field
       | Quote_in_quoted_field, ',' -> end_field (); state := Start_field
       | Quote_in_quoted_field, '\r' -> ()
       | Quote_in_quoted_field, '\n' -> end_row (); state := Start_field
       | Quote_in_quoted_field, _ -> state := In_field (* malformed, recover *)
      );
      incr i
    done;

    if Buffer.length buf > 0 || !current_row <> [] || !state <> Start_field then
      end_row ();

    List.rev !rows


(* ---------- Typed column representation ---------- *)
type column = {
  dtype : Dtype.t;
  values : Value.t option array;
}

type t = {
  column_names : string list;
  columns : (string, column) Hashtbl.t;
  n_rows : int;
}

(* ---------- Type inference ---------- *)

let is_null na_values s = List.mem s na_values
   
let infer_cell_type ~na_values (s : string) : Dtype.t option =
  let s = String.trim s in
  if is_null na_values s then None
  else
    let dtype =
      match int_of_string_opt s with
      | Some _ -> Dtype.TInt
      | None ->
        match float_of_string_opt s with
        | Some _ -> Dtype.TFloat
        | None ->
          match String.lowercase_ascii s with
          | "true" | "false" -> Dtype.TBool
          | _ -> Dtype.TStr
    in
    Some dtype

let infer_column_type ~na_values (values : string list) : Dtype.t =
  List.fold_left
    (fun acc v ->
       match infer_cell_type ~na_values v with
       | None -> acc (* null, doesn't constrain the type *)
       | Some t -> (match acc with None -> Some t | Some acc -> Some (Dtype.widen acc t)))
    None values
  |> Option.value ~default:Dtype.TStr (* all-null column defaults to Str *)

(* ---------- Parsing a single cell into a typed Value.t ---------- *)

let parse_cell (dtype : Dtype.t) ~na_values (s : string) : Value.t option =
  let s = String.trim s in
  if is_null na_values s then None
  else
    match dtype with
    | Dtype.TInt ->
      (match int_of_string_opt s with
       | Some i -> Some (Value.int i)
       | None -> None (* malformed cell in an inferred-int column *))
    | Dtype.TFloat ->
      (match float_of_string_opt s with
       | Some f -> Some (Value.float f)
       | None -> None)
    | Dtype.TBool ->
      (match String.lowercase_ascii s with
       | "true" -> Some (Value.bool true)
       | "false" -> Some (Value.bool false)
       | _ -> None)
    | Dtype.TStr -> Some (Value.str s)

let build_column (dtype : Dtype.t) ~na_values (values : string list) : column =
  {
    dtype;
    values = Array.of_list (List.map (parse_cell dtype ~na_values) values);
  }

(* ---------- Full CSV -> table ---------- *)

let read_csv ?(has_header = true) ?(dtypes = []) ?(na_values = [""]) (path : string) : t =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;

  let rows = parse_string content in
  match rows with
  | [] -> { column_names = []; columns = Hashtbl.create 0; n_rows = 0 }
  | first_row :: rest ->
    let column_names, data_rows =
      if has_header then first_row, rest
      else
        List.mapi (fun i _ -> Printf.sprintf "column_%d" i) first_row,
        first_row :: rest
    in
    let n_cols = List.length column_names in

    (* transpose row-major -> column-major, accumulating in reverse for O(1) cons *)
    let col_values = Array.make n_cols [] in
    List.iter
      (fun row ->
         List.iteri
           (fun i cell -> if i < n_cols then col_values.(i) <- cell :: col_values.(i))
           row)
      data_rows;

    let columns = Hashtbl.create n_cols in
    List.iteri
      (fun i name ->
         let values = List.rev col_values.(i) in
         let dtype =
           match List.assoc_opt name dtypes with
           | Some declared -> declared
           | None -> infer_column_type ~na_values values
         in
         Hashtbl.replace columns name (build_column dtype ~na_values values))
      column_names;

    { column_names; columns; n_rows = List.length data_rows }
