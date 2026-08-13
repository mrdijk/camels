type t = (string * Value.t) list

let make fields = fields
let get row name = List.assoc name row
