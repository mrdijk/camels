open Camels

let ( let* ) = Result.bind

type order = {
  id : int;
  region : string;
  amount : float;
}

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
  Lazyframe.read_csv_lazy "btcusd_1-min_data.csv"
  |> fun df -> Lazyframe.filter df Expr.( col "Volume" >. F 0.0 )

let () =
  (* print_endline (Schema.to_string (Lazyframe.schema pipeline)); *)
  (* print_result (Lazyframe.collect pipeline); *)
  let input = [| Some (Value.VInt 1); None; Some (Value.VInt 3) |] in
  assert (Series.to_values (Series.of_values input) = input);
  (* Printf.printf "Not optimized:\n%s\n\n" (Lazyframe.explain pipeline); *)
  (* Printf.printf "Optimized:\n%s\n\n" (Lazyframe.explain ~optimize:true pipeline); *)
  let input2 = [| None; None |] in
  assert (Series.to_values (Series.of_values input2) = input2)
