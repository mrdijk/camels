exception Camels_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Camels_error s)) fmt
