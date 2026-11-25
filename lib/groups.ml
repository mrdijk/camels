module S = Series

(* ------------------------------------------------------------------ *)
(* int -> dense id dictionary                                          *)
(* ------------------------------------------------------------------ *)

module Intdict = struct
  type t = {
    dense : bool;
    lo : int;
    table : int array;  (* dense: key - lo -> id, -1 if absent *)
    mutable keys : int array;  (* hash: open addressing, ids.(slot) < 0 = empty *)
    mutable ids : int array;
    mutable mask : int;
    mutable stored : int;
    mutable next : int;
  }

  let hash_int k =
    let h = k * 0x2545F4914F6CDD1D in
    h lxor (h lsr 29)

  let pow2_at_least n =
    let s = ref 16 in
    while !s < n do s := !s * 2 done;
    !s

  (* [lo..hi] bounds the keys that will be inserted, [n] is their count. *)
  let create ~lo ~hi ~n =
    let range = hi - lo + 1 in
    if hi >= lo && range > 0 && range <= (4 * n) + 1024 then
      { dense = true; lo; table = Array.make range (-1); keys = [||]; ids = [||];
        mask = 0; stored = 0; next = 0 }
    else
      let size = pow2_at_least 32 in
      { dense = false; lo = 0; table = [||]; keys = Array.make size 0;
        ids = Array.make size (-1); mask = size - 1; stored = 0; next = 0 }

  let slot keys ids mask k =
    let i = ref (hash_int k land mask) in
    while ids.(!i) >= 0 && keys.(!i) <> k do i := (!i + 1) land mask done;
    !i

  let grow d =
    let size = 2 * (d.mask + 1) in
    let keys = Array.make size 0 and ids = Array.make size (-1) in
    let mask = size - 1 in
    Array.iteri
      (fun i id ->
        if id >= 0 then begin
          let s = slot keys ids mask d.keys.(i) in
          keys.(s) <- d.keys.(i);
          ids.(s) <- id
        end)
      d.ids;
    d.keys <- keys;
    d.ids <- ids;
    d.mask <- mask

  (* id of [k], or -1 *)
  let find d k =
    if d.dense then begin
      let j = k - d.lo in
      if j < 0 || j >= Array.length d.table then -1 else d.table.(j)
    end
    else begin
      let s = slot d.keys d.ids d.mask k in
      d.ids.(s)
    end

  (* id of [k], assigning the next id if it is new *)
  let find_or_add d k =
    if d.dense then begin
      let j = k - d.lo in
      let id = d.table.(j) in
      if id >= 0 then id
      else begin
        let id = d.next in
        d.table.(j) <- id;
        d.next <- id + 1;
        id
      end
    end
    else begin
      let s = slot d.keys d.ids d.mask k in
      if d.ids.(s) >= 0 then d.ids.(s)
      else begin
        let id = d.next in
        d.keys.(s) <- k;
        d.ids.(s) <- id;
        d.next <- id + 1;
        d.stored <- d.stored + 1;
        if 2 * d.stored > d.mask then grow d;
        id
      end
    end

  (* an id that no key maps to (used for the null group) *)
  let reserve d =
    let id = d.next in
    d.next <- id + 1;
    id

  let count d = d.next
end

module StrTbl = Hashtbl.Make (struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

module FloatTbl = Hashtbl.Make (struct
  type t = float
  let equal (a : float) (b : float) = compare a b = 0
  let hash = Hashtbl.hash
end)

(* ------------------------------------------------------------------ *)
(* Factorizing one column: dense codes in order of first appearance    *)
(* ------------------------------------------------------------------ *)

let int_bounds (x : int array) valid =
  let lo = ref max_int and hi = ref min_int in
  for i = 0 to Array.length x - 1 do
    if valid i then begin
      let v = x.(i) in
      if v < !lo then lo := v;
      if v > !hi then hi := v
    end
  done;
  (!lo, !hi)

let factorize_ints (x : int array) valid =
  let n = Array.length x in
  let lo, hi = int_bounds x valid in
  let d = Intdict.create ~lo ~hi ~n in
  let codes = Array.make n 0 in
  let null_code = ref (-1) in
  for i = 0 to n - 1 do
    if valid i then codes.(i) <- Intdict.find_or_add d x.(i)
    else begin
      if !null_code < 0 then null_code := Intdict.reserve d;
      codes.(i) <- !null_code
    end
  done;
  (codes, Intdict.count d)

let factorize_strings (x : string array) valid =
  let n = Array.length x in
  let tbl = StrTbl.create 64 in
  let codes = Array.make n 0 in
  let next = ref 0 and null_code = ref (-1) in
  for i = 0 to n - 1 do
    if valid i then begin
      let v = x.(i) in
      match StrTbl.find tbl v with
      | c -> codes.(i) <- c
      | exception Not_found ->
          let c = !next in
          incr next;
          StrTbl.add tbl v c;
          codes.(i) <- c
    end
    else begin
      if !null_code < 0 then begin null_code := !next; incr next end;
      codes.(i) <- !null_code
    end
  done;
  (codes, !next)

let factorize_floats (x : float array) valid =
  let n = Array.length x in
  let tbl = FloatTbl.create 64 in
  let codes = Array.make n 0 in
  let next = ref 0 and null_code = ref (-1) in
  for i = 0 to n - 1 do
    if valid i then begin
      let v = x.(i) in
      match FloatTbl.find tbl v with
      | c -> codes.(i) <- c
      | exception Not_found ->
          let c = !next in
          incr next;
          FloatTbl.add tbl v c;
          codes.(i) <- c
    end
    else begin
      if !null_code < 0 then begin null_code := !next; incr next end;
      codes.(i) <- !null_code
    end
  done;
  (codes, !next)

let factorize_bools (m : Bitmap.t) n valid =
  let slots = [| -1; -1; -1 |] in  (* false, true, null *)
  let next = ref 0 in
  let codes = Array.make n 0 in
  for i = 0 to n - 1 do
    let k = if not (valid i) then 2 else if Bitmap.get m i then 1 else 0 in
    if slots.(k) < 0 then begin slots.(k) <- !next; incr next end;
    codes.(i) <- slots.(k)
  done;
  (codes, !next)

let factorize (s : S.t) : int array * int =
  let valid = S.is_valid s in
  match s.data with
  | S.Ints x -> factorize_ints x valid
  | S.Floats x -> factorize_floats x valid
  | S.Strings x -> factorize_strings x valid
  | S.Bools m -> factorize_bools m (S.length s) valid

(* combine two code columns into one, re-densified *)
let combine (c1, n1) (c2, n2) =
  let n = Array.length c1 in
  let comp = Array.make n 0 in
  for i = 0 to n - 1 do comp.(i) <- (c1.(i) * n2) + c2.(i) done;
  let d = Intdict.create ~lo:0 ~hi:((n1 * n2) - 1) ~n in
  let codes = Array.make n 0 in
  for i = 0 to n - 1 do codes.(i) <- Intdict.find_or_add d comp.(i) done;
  (codes, Intdict.count d)

(* ------------------------------------------------------------------ *)
(* Groups                                                              *)
(* ------------------------------------------------------------------ *)

type t = {
  codes : int array;
  ngroups : int;
  first_rows : int array;
  mutable rows : int array array option;
}

let of_keys (keys : S.t list) (n : int) : t =
  let codes, ngroups =
    match keys with
    | [] -> (Array.make n 0, 1)
    | k :: rest -> List.fold_left (fun acc s -> combine acc (factorize s)) (factorize k) rest
  in
  let first_rows = Array.make ngroups (-1) in
  for i = n - 1 downto 0 do first_rows.(codes.(i)) <- i done;
  { codes; ngroups; first_rows; rows = None }

let sizes g =
  let c = Array.make g.ngroups 0 in
  Array.iter (fun k -> c.(k) <- c.(k) + 1) g.codes;
  c

let last_rows g =
  let last = Array.make g.ngroups (-1) in
  Array.iteri (fun i k -> last.(k) <- i) g.codes;
  last

let rows g =
  match g.rows with
  | Some r -> r
  | None ->
      let sz = sizes g in
      let out = Array.map (fun c -> Array.make c 0) sz in
      let pos = Array.make g.ngroups 0 in
      Array.iteri
        (fun i k ->
          out.(k).(pos.(k)) <- i;
          pos.(k) <- pos.(k) + 1)
        g.codes;
      g.rows <- Some out;
      out

(* ------------------------------------------------------------------ *)
(* Join keys                                                           *)
(* ------------------------------------------------------------------ *)

let int_pair (x : int array) lv (y : int array) rv =
  let nl = Array.length x and nr = Array.length y in
  let lo, hi = int_bounds y rv in
  let d = Intdict.create ~lo ~hi ~n:nr in
  let rc = Array.make nr (-1) in
  for j = 0 to nr - 1 do if rv j then rc.(j) <- Intdict.find_or_add d y.(j) done;
  let lc = Array.make nl (-1) in
  for i = 0 to nl - 1 do if lv i then lc.(i) <- Intdict.find d x.(i) done;
  (lc, rc, Intdict.count d)

let string_pair (x : string array) lv (y : string array) rv =
  let nl = Array.length x and nr = Array.length y in
  let tbl = StrTbl.create 64 and next = ref 0 in
  let rc = Array.make nr (-1) in
  for j = 0 to nr - 1 do
    if rv j then begin
      let v = y.(j) in
      match StrTbl.find tbl v with
      | c -> rc.(j) <- c
      | exception Not_found ->
          StrTbl.add tbl v !next;
          rc.(j) <- !next;
          incr next
    end
  done;
  let lc = Array.make nl (-1) in
  for i = 0 to nl - 1 do
    if lv i then match StrTbl.find tbl x.(i) with c -> lc.(i) <- c | exception Not_found -> ()
  done;
  (lc, rc, !next)

let float_pair (x : float array) lv (y : float array) rv =
  let nl = Array.length x and nr = Array.length y in
  let tbl = FloatTbl.create 64 and next = ref 0 in
  let rc = Array.make nr (-1) in
  for j = 0 to nr - 1 do
    if rv j then begin
      let v = y.(j) in
      match FloatTbl.find tbl v with
      | c -> rc.(j) <- c
      | exception Not_found ->
          FloatTbl.add tbl v !next;
          rc.(j) <- !next;
          incr next
    end
  done;
  let lc = Array.make nl (-1) in
  for i = 0 to nl - 1 do
    if lv i then match FloatTbl.find tbl x.(i) with c -> lc.(i) <- c | exception Not_found -> ()
  done;
  (lc, rc, !next)

let bool_pair (x : Bitmap.t) lv nl (y : Bitmap.t) rv nr =
  let code m v i = if not (v i) then -1 else if Bitmap.get m i then 1 else 0 in
  (Array.init nl (code x lv), Array.init nr (code y rv), 2)

let pair_codes (l : S.t) (r : S.t) =
  let lv = S.is_valid l and rv = S.is_valid r in
  match (l.data, r.data) with
  | S.Ints x, S.Ints y -> int_pair x lv y rv
  | (S.Ints _ | S.Floats _), (S.Ints _ | S.Floats _) ->
      let fl = S.to_float l and fr = S.to_float r in
      (match (fl.data, fr.data) with
       | S.Floats x, S.Floats y -> float_pair x lv y rv
       | _ -> assert false)
  | S.Strings x, S.Strings y -> string_pair x lv y rv
  | S.Bools x, S.Bools y -> bool_pair x lv (S.length l) y rv (S.length r)
  | _ ->
      Err.fail "join key type mismatch: %S is %s but %S is %s" l.name
        (Dtype.to_string (S.dtype l)) r.name (Dtype.to_string (S.dtype r))

let combine_join (lc1, rc1, n1) (lc2, rc2, n2) =
  let nl = Array.length lc1 and nr = Array.length rc1 in
  let d = Intdict.create ~lo:0 ~hi:((n1 * n2) - 1) ~n:nr in
  let rc = Array.make nr (-1) in
  for j = 0 to nr - 1 do
    if rc1.(j) >= 0 && rc2.(j) >= 0 then rc.(j) <- Intdict.find_or_add d ((rc1.(j) * n2) + rc2.(j))
  done;
  let lc = Array.make nl (-1) in
  for i = 0 to nl - 1 do
    if lc1.(i) >= 0 && lc2.(i) >= 0 then lc.(i) <- Intdict.find d ((lc1.(i) * n2) + lc2.(i))
  done;
  (lc, rc, Intdict.count d)

let join_codes (lkeys : S.t list) (rkeys : S.t list) =
  match (lkeys, rkeys) with
  | l :: lrest, r :: rrest ->
      List.fold_left2
        (fun acc l r -> combine_join acc (pair_codes l r))
        (pair_codes l r) lrest rrest
  | _ -> Err.fail "join requires at least one key"
