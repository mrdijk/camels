(** Eager, column-oriented dataframe. *)

type t = { columns : Series.t array; height : int }

let empty = { columns = [||]; height = 0 }

let create (cols : Series.t list) =
  match cols with
  | [] -> empty
  | c :: _ ->
      let h = Series.length c in
      let seen = Hashtbl.create 8 in
      List.iter
        (fun (s : Series.t) ->
          if Series.length s <> h then
            Err.fail "column %S has length %d but expected %d" s.name
              (Series.length s) h;
          if Hashtbl.mem seen s.name then Err.fail "duplicate column name %S" s.name;
          Hashtbl.add seen s.name ())
        cols;
      { columns = Array.of_list cols; height = h }

let of_columns pairs = create (List.map (fun (n, d) -> Series.make n d) pairs)
let height t = t.height
let width t = Array.length t.columns

let column_names t =
  Array.to_list (Array.map (fun (s : Series.t) -> s.name) t.columns)

let get_column t name =
  match Array.find_opt (fun (s : Series.t) -> s.name = name) t.columns with
  | Some s -> s
  | None ->
      Err.fail "ColumnNotFound: %S (available: [%s])" name
        (String.concat ", " (column_names t))

let select_columns t names =
  { t with columns = Array.of_list (List.map (get_column t) names) }

let take t idx =
  { columns = Array.map (fun s -> Series.take s idx) t.columns; height = Array.length idx }

let take_opt t idx =
  { columns = Array.map (fun s -> Series.take_opt s idx) t.columns; height = Array.length idx }

let filter t (mask : bool array) =
  let idx = ref [] in
  for i = Array.length mask - 1 downto 0 do
    if mask.(i) then idx := i :: !idx
  done;
  take t (Array.of_list !idx)

(** Replace the column of the same name in place, or append it. *)
let with_column t (s : Series.t) =
  if width t > 0 && Series.length s <> t.height then
    Err.fail "column %S has length %d but frame height is %d" s.name
      (Series.length s) t.height;
  if Array.exists (fun (c : Series.t) -> c.name = s.name) t.columns then
    { t with columns = Array.map (fun (c : Series.t) -> if c.name = s.name then s else c) t.columns }
  else { columns = Array.append t.columns [| s |]; height = Series.length s }

let to_rows t =
  List.init t.height (fun r ->
      Array.to_list (Array.map (fun (s : Series.t) -> s.data.(r)) t.columns))

let to_string t =
  let ncols = width t in
  let header = Array.map (fun (s : Series.t) -> s.name) t.columns in
  let types =
    Array.map (fun s -> Value.dtype_to_string (Series.dtype s)) t.columns
  in
  let cells =
    Array.init t.height (fun r ->
        Array.map (fun (s : Series.t) -> Value.to_string s.data.(r)) t.columns)
  in
  let widths =
    Array.init ncols (fun c ->
        Array.fold_left
          (fun w row -> max w (String.length row.(c)))
          (max (String.length header.(c)) (String.length types.(c)))
          cells)
  in
  let b = Buffer.create 256 in
  let sep () =
    Array.iter (fun w -> Buffer.add_string b ("+" ^ String.make (w + 2) '-')) widths;
    Buffer.add_string b "+\n"
  in
  let row cells =
    Array.iteri
      (fun c s ->
        Buffer.add_string b
          (Printf.sprintf "| %s%s " s (String.make (widths.(c) - String.length s) ' ')))
      cells;
    Buffer.add_string b "|\n"
  in
  Buffer.add_string b (Printf.sprintf "shape: (%d, %d)\n" t.height ncols);
  sep (); row header; row types; sep ();
  Array.iter row cells;
  sep ();
  Buffer.contents b

let print t = print_string (to_string t)

