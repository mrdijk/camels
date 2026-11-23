open Camels
module L = Lazyframe

let orders =
  Io.read_csv_string
    "order_id,customer,amount,status\n\
     1,ann,10.5,paid\n\
     2,bob,20,paid\n\
     3,ann,5,refunded\n\
     4,cy,,paid\n\
     5,bob,7.25,paid\n"

let customers =
  Dataframe.create
    [
      Series.of_strings "customer" [| "ann"; "bob"; "cy" |];
      Series.of_strings "country" [| "NL"; "DE"; "NL" |];
    ]

let () =
  let open Expr in
  L.of_df orders
  |> L.filter (col "status" = str "paid" && is_not_null (col "amount"))
  |> L.join ~on:[ "customer" ] (L.of_df customers)
  |> L.with_column (col "amount" * float 1.21 |> alias "gross")
  |> L.group_by [ col "country" ]
  |> L.agg [ len () |> alias "orders"; sum (col "gross") |> alias "revenue" ]
  |> L.collect
  |> Dataframe.print
