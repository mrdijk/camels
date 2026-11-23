open Camels
module L = Lazyframe

let time f = let t0 = Unix.gettimeofday () in let r = f () in (r, (Unix.gettimeofday () -. t0) *. 1000.)
let best n f = let b = ref infinity in for _ = 1 to n do let _, t = time f in if t < !b then b := t done; !b
let () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let _, t_full = time (fun () -> Io.read_csv_string text) in
  let _, t_schema = time (fun () ->
    Io.read_csv_string ~infer_rows:0 ~schema:[("id",Dtype.Int64);("user_id",Dtype.Int64);("category",Dtype.Utf8);("amount",Dtype.Float64);("qty",Dtype.Int64)] text) in
  Printf.printf "read_csv_string, inferred types       %7.1f ms\n" t_full;
  Printf.printf "read_csv_string, schema given         %7.1f ms\n" t_schema;
  let o = Io.read_csv_string text in
  let q = L.of_df o in
  let open Expr in
  let run name q = Printf.printf "%-38s %7.1f ms\n%!" name (best 3 (fun () -> L.collect q)) in
  run "select [col id] (pass-through)" (q |> L.select [ col "id" ]);
  run "group_by user_id, len only" (q |> L.group_by [ col "user_id" ] |> L.agg [ len () |> alias "n" ]);
  run "group_by user_id, sum amount" (q |> L.group_by [ col "user_id" ] |> L.agg [ sum (col "amount") |> alias "t" ]);
  run "group_by category, len only" (q |> L.group_by [ col "category" ] |> L.agg [ len () |> alias "n" ]);
  run "filter amount>500 (float col only)" (q |> L.filter (col "amount" > float 500.));
  run "filter category=c3 (string col only)" (q |> L.filter (col "category" = str "c3"))
