type t = { length : int; columns : (string * Series.t) list }

let length b = b.length
let columns b = b.columns
let get_column b name = List.assoc name b.columns

let of_columns cols =
  match cols with
  | [] -> { length = 0; columns = [] }
  | (_, s) :: _ -> { length = Series.length s; columns = cols }

let of_rows (rows : Row.t list) : t =
  match rows with
  | [] -> { length = 0; columns = [] }
  | first :: _ ->
      let names = List.map fst first in
      let columns =
        List.map
          (fun name ->
            let values = Array.of_list (List.map (fun row -> Row.get row name) rows) in
            (name, Series.of_values values))
          names
      in
      { length = List.length rows; columns }

let apply_mask (b : t) (mask : bool array) : t =
  let columns = List.map (fun (name, s) -> (name, Series.apply_mask s mask)) b.columns in
  let length = match columns with [] -> 0 | (_, s) :: _ -> Series.length s in
  { length; columns }

let to_rows (b : t) : Row.t list =
  List.init b.length (fun i ->
      Row.make (List.map (fun (name, s) -> (name, Series.get s i)) b.columns))
