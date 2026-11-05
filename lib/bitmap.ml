type t = Bytes.t

let create n v = Bytes.make n (if v then '\001' else '\000')
let length = Bytes.length
let get t i = Bytes.unsafe_get t i <> '\000'
let set t i v = Bytes.unsafe_set t i (if v then '\001' else '\000')

let and_ a b =
  Bytes.init (Bytes.length a) (fun i ->
    if get a i && get b i then '\001' else '\000')

let count_set t =
  let c = ref 0 in
  Bytes.iter (fun ch -> if ch <> '\000' then incr c) t;
  !c
