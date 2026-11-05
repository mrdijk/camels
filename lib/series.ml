(** A named column of values. *)

type buffer =
  | Ints of int array
  | Floats of float array
  | Strings of string array
  | Bools of Bitmap.t

type t = {
  name : string;
  data : buffer;
  validity : Bitmap.t option
}

let buffer_length = function
  | Ints a -> Array.length a
  | Floats a -> Array.length a
  | Strings a -> Array.length a
  | Bools b -> Bitmap.length b

let make name data validity =
  (match validity with
   | Some v when Bitmap.length v <> buffer_length data ->
     Err.fail "validity length %d does not match data lenght %d"
       (Bitmap.length v) (buffer_length data)
   | _ -> ());
  { name; data; validity }

let length s = buffer_length s.data
let rename s name = { s with name }

(* ---- from plain arrays: no nulls, so validity = None ---- *)

let of_ints name a = { name; data = Ints (Array.copy a); validity = None }
let of_floats name a = { name; data = Floats (Array.copy a); validity = None }
let of_strings name a = { name; data = Strings (Array.copy a); validity = None }
let of_bools name a =
  let bm = Bitmap.create (Array.length a) false in
  Array.iteri (fun i b -> if b then Bitmap.set bm i true) a;
  { name;data = Bools bm; validity = None}

(* ---- from option arrays ---- *)

(* Generic builder. [alloc n] makes a buffer whose slots hold a placeholder,
   [store buf i x] writes a present value, [wrap] tags the buffer. The mask is
   only kept if at least one element is [None]. *)
let of_options ~alloc ~store ~wrap name (a : 'a option array) =
  let n = Array.length a in
  let buf = alloc n in
  let valid = Bitmap.create n true in
  let has_null = ref false in
  Array.iteri (fun i -> function
    | Some x -> store buf i x
    | None ->
        Bitmap.set valid i false;
        has_null := true)
    a;
  { name; data = wrap buf; validity = (if !has_null then Some valid else None) }

let of_opt_ints =
  of_options ~alloc:(fun n -> Array.make n 0) ~store:Array.set ~wrap:(fun a -> Ints a)

let of_opt_floats =
  of_options ~alloc:(fun n -> Array.make n 0.0) ~store:Array.set ~wrap:(fun a -> Floats a)

let of_opt_strings =
  of_options ~alloc:(fun n -> Array.make n "") ~store:Array.set ~wrap:(fun a -> Strings a)

let of_opt_bools =
  of_options
    ~alloc:(fun n -> Bitmap.create n false)
    ~store:Bitmap.set
    ~wrap:(fun b -> Bools b)

let is_valid s i = match s.validity with None -> true | Some v -> Bitmap.get v i

let null_count s =
  match s.validity with
  | None -> 0
  | Some v -> length s - Bitmap.count_set v

(* [gather ~allow_neg s idx] builds a series whose row [j] is row [idx.(j)] of
   [s]. With [allow_neg], a negative index produces a null row (its data slot
   holds a placeholder). Out-of-range indices raise [Invalid_argument]; so do
   negative ones when [allow_neg] is false. *)
let gather ~allow_neg s idx =
  let n = Array.length idx in
  let neg i = allow_neg && i < 0 in
  let data =
    match s.data with
    | Ints a -> Ints (Array.init n (fun j -> let i = idx.(j) in if neg i then 0 else a.(i)))
    | Floats a -> Floats (Array.init n (fun j -> let i = idx.(j) in if neg i then 0.0 else a.(i)))
    | Strings a -> Strings (Array.init n (fun j -> let i = idx.(j) in if neg i then "" else a.(i)))
    | Bools b ->
      let out = Bitmap.create n false in
      Array.iteri (fun j i -> if (not (neg i)) && Bitmap.get b i then Bitmap.set out j true) idx;
      Bools out
  in
  let validity =
    match (s.validity, allow_neg) with
    | None, false -> None
    | _ ->
      let out = Bitmap.create n true in
      let has_null = ref false in
      Array.iteri (fun j i ->
        let ok = (not (neg i)) && is_valid s i in
        if not ok then begin
          Bitmap.set out j false;
          has_null := true
          end)
        idx;
      if !has_null then Some out else None
  in
  { name = s.name; data; validity }

let take s idx = gather ~allow_neg:false s idx

(** Like [take] but a negative index produces [Null]. *)
let take_opt s idx = gather ~allow_neg:true s idx

let of_values ?dtype name (vs : Value.t array) =
  let ty =
    match dtype with Some _ -> dtype | None ->
    Array.fold_left (fun acc v ->
      match (acc, Value.dtype_of v) with
      | acc, None -> acc
      | None, t -> t
      | Some a, Some b -> (
        match Dtype.supertype a b with
        | Some t -> Some t
        | None -> Err.fail "column %S mixes %s and %s" name (Dtype.to_string a) (Dtype.to_string b)))
    None vs
  in
  let bad v = Err.fail "column %S: unexpected value %s" name (Value.to_string v) in
  match ty with
  | None | Some Dtype.Int64 ->
    of_opt_ints name (Array.map (function Value.Null -> None | Value.Int i -> Some i | v -> bad v) vs)
  | Some Dtype.Float64 ->
    of_opt_floats name
      (Array.map (function
        | Value.Null -> None
        | Value.Int i -> Some (float_of_int i)
        | Value.Float f -> Some f
        | v -> bad v)
        vs)
  | Some Dtype.Utf8 ->
    of_opt_strings name (Array.map (function Value.Null -> None | Value.Str s -> Some s | v -> bad v) vs)
  | Some Dtype.Boolean ->
    of_opt_bools name (Array.map (function Value.Null -> None | Value.Bool b -> Some b | v -> bad v) vs)

let dtype s : Dtype.t =
  match s.data with
  | Ints _-> Int64
  | Floats _-> Float64
  | Strings _-> Utf8
  | Bools _ -> Boolean

let get s i : Value.t =
  if not (is_valid s i ) then Null
  else
    match s.data with
    | Ints a -> Int a.(i)
    | Floats a -> Float a.(i)
    | Strings a -> Str a.(i)
    | Bools b -> Bool (Bitmap.get b i)

(** [n] copies of a scalar. A [Null] needs a type: [dtype] (default Int64). *)
let of_scalar ?dtype name n (v : Value.t) =
  match v with
  | Int i -> { name; data = Ints (Array.make n i); validity = None }
  | Float f -> { name; data = Floats (Array.make n f); validity = None }
  | Str s -> { name; data = Strings (Array.make n s); validity = None }
  | Bool b -> { name; data = Bools (Bitmap.create n b); validity = None }
  | Null ->
      let data =
        match dtype with
        | Some Dtype.Float64 -> Floats (Array.make n 0.0)
        | Some Dtype.Utf8 -> Strings (Array.make n "")
        | Some Dtype.Boolean -> Bools (Bitmap.create n false)
        | Some Dtype.Int64 | None -> Ints (Array.make n 0)
      in
      { name; data; validity = Some (Bitmap.create n false) }
