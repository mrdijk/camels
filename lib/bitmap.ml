type t = Bytes.t

let create n v = Bytes.make n (if v then '\001' else '\000')
let length = Bytes.length
let get t i = Bytes.get t i <> '\000'
let set t i v = Bytes.set t i (if v then '\001' else '\000')

let and_ a b =
  let n = Bytes.length a in
  if Bytes.length b <> n then invalid_arg "Bitmap.and_";
  let out = Bytes.make n '\000' in
  for i = 0 to n - 1 do
    if Bytes.unsafe_get a i <> '\000' && Bytes.unsafe_get b i <> '\000' then
      Bytes.unsafe_set out i '\001'
  done;
  out

let count_set t =
  let c = ref 0 in
  for i = 0 to Bytes.length t - 1 do
    if Bytes.unsafe_get t i <> '\000' then incr c
  done;
  !c

let concat ts = Bytes.concat Bytes.empty ts
let sub t pos len = Bytes.sub t pos len

let of_bytes (b : Bytes.t) = b
