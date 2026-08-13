open Camels

let ( => ) k v = (k, v)

let data =
    Value.(
      [ [ "order_id" => int 1; "region" => str "EU";   "amount" => float 250.0 ];
        [ "order_id" => int 2; "region" => str "US";   "amount" => float 45.0 ];
        [ "order_id" => int 3; "region" => str "EU";   "amount" => float 180.0 ];
        [ "order_id" => int 4; "region" => str "US";   "amount" => float 320.0 ];
        [ "order_id" => int 5; "region" => str "APAC"; "amount" => float 90.0 ];
        [ "order_id" => int 6; "region" => str "US";   "amount" => float 150.0 ] ])

let () =
  let orders = Lazyframe.of_rows_chunked data ~chunk_size:3 in
  let query = Expr.(col "amount" >. F 100.0) in
  let result = Lazyframe.(collect (filter orders query)) in
  Printf.printf "query: %s\n" (Expr.to_string query);
  List.iter
    (fun row ->
      List.iter
        (fun (k, v) -> Printf.printf "%s=%s " k (Value.to_string v))
        row;
      print_newline ())
    result
