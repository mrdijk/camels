open Camels

let ( let* ) = Result.bind

type order = {
  id : int;
  region : string;
  amount : float;
}

let parse_id raw_id =
  raw_id
  |> int_of_string_opt
  |> Option.to_result ~none:`Invalid_id

let parse_region = function
  | "" -> Error `Missing_name
  | n -> Ok n

let parse_amount raw_amount =
  raw_amount
  |> float_of_string_opt
  |> Option.to_result ~none:`Invalid_amount

let parse_row row =
  let* id = Csv.Row.find row "id" |> parse_id
  in
  let* region = Csv.Row.find row "region" |> parse_region 
  in
  let* amount = Csv.Row.find row "amount" |> parse_amount
  in
  Ok { id; region; amount }

let parse_orders csv_str =
  csv_str
  |> Csv.of_string ~has_header:true
  |> Csv.Rows.input_all
  |> List.map parse_row

let sample_csv =
  {|"id","region","amount",
  "1","EU","250.0",
  "2","US","45.5",
  "3","APAC","90.0",|}

let columns = [ "order_id"; "region"; "amount" ]

let data : Row.t list =
  Value.(
    [ [| int 1; str "EU"; float 250.0 |];
      [| int 2; str "US"; float 45.5 |];
      [| int 3; str "EU"; float 180.0 |];
      [| int 4; str "US"; float 320.0 |];
      [| int 5; str "APAC"; float 90.0 |];
      [| int 6; str "US"; float 150.0 |] ])



let print_result result =
    List.iter
    (fun row -> Array.iter (fun v -> Printf.printf "%s " (Value.to_string v)) row; print_newline ())
    result

let () =
  parse_orders sample_csv
  |> List.iter (function
    | Ok order ->
        Printf.printf "%d %s %f\n"
          order.id
          order.region
          order.amount 
    | Error `Invalid_id ->
        print_endline "Invalid_id"
    | Error `Invalid_amount ->
        print_endline "Unknown_amount"
    | Error `Missing_name ->
        print_endline "Missing_region")

let () =
  let result =
    Lazyframe.of_rows data ~columns
    |> fun df -> Lazyframe.filter df Expr.(col "amount" >. F 45.0)
    (* |> fun df -> Lazyframe.select df [ "region"; "amount" ] *)
    |> Lazyframe.collect
  in
  print_result result 
