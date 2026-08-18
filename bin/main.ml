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

let order_columns = [ "order_id"; "customer_id"; "amount" ]
let orders : Row.t list =
  Value.(
      [ Row.some_of [| int 1; int 101; float 250.0 |];
        Row.some_of [| int 2; int 102; float 45.0 |];
        Row.some_of [| int 3; int 103; float 180.0 |];
        Row.some_of [| int 4; int 104; float 320.0 |];
        Row.some_of [| int 5; int 105; float 90.0 |];
        Row.some_of [| int 6; int 106; float 150.0 |] ])

let customer_columms = [ "customer_id"; "name"]
let customers : Row.t list =
  Value.(
    [Row.some_of [| int 101; str "Alice" |];
     Row.some_of [| int 102; str "Bob" |];
     Row.some_of [| int 103; str "Carol" |]]
  )
let print_result (rows : Row.t list) : unit =
  List.iter
    (fun (row : Row.t) ->
      Array.iter
        (fun v -> Printf.printf "%s " (Option.fold ~none:"null" ~some:Value.to_string v))
        row;
      print_newline ())
    rows

 let pipeline =
  Lazyframe.of_rows orders ~columns:order_columns
  |> fun df -> Lazyframe.filter df Expr.(col "amount" >. F 100.0)
  |> fun df -> Lazyframe.with_column df "tax" Expr.(col "amount" *. F 0.2)
  |> fun df -> Lazyframe.select df [ "order_id"; "amount"; "tax" ]

let orders_df = Lazyframe.of_rows orders ~columns:order_columns
let customer_df = Lazyframe.of_rows customers ~columns:customer_columms

let result =
  Lazyframe.join orders_df customer_df ~left_on:[ "customer_id" ] ~right_on:[ "customer_id" ] ~how:Plan.Inner
  |> fun df -> Lazyframe.filter df Expr.(col "name" =. S "Alice")
  |> fun df -> Lazyframe.select df [ "order_id"; "customer_id"; "amount" ]
  
let () =
  print_endline (Schema.to_string (Lazyframe.schema result));
  print_result (Lazyframe.collect result);
  Printf.printf "Not optimized:\n%s\n\n" (Lazyframe.explain result);
  Printf.printf "Optimized:\n%s\n\n" (Lazyframe.explain ~optimize:true result);
