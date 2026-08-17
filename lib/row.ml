type t = Value.t option array

let get row i = row.(i)
let some_of (vs : Value.t array) : t = Array.map Option.some vs
