(* Usage: bench.exe orders.csv users.csv
   Wall-clock timings; best of 3 for queries, single run for the CSV read. *)
open Camels
module L = Lazyframe

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. t0)

let best n f =
  let best = ref infinity and last = ref None in
  for _ = 1 to n do
    let r, t = time f in
    last := Some r;
    if t < !best then best := t
  done;
  (Option.get !last, !best)

let report name t rows = Printf.printf "%-34s %8.1f ms   (%d rows)\n%!" name (t *. 1000.) rows

let () =
  let orders_path = Sys.argv.(1) and users_path = Sys.argv.(2) in
  let orders, t = time (fun () -> Io.read_csv orders_path) in
  report "read_csv orders (1M x 5)" t (Dataframe.height orders);
  let users, t = time (fun () -> Io.read_csv users_path) in
  report "read_csv users (10k x 3)" t (Dataframe.height users);
  let o = L.of_df orders and u = L.of_df users in
  let run name q =
    let df, t = best 3 (fun () -> L.collect q) in
    report name t (Dataframe.height df)
  in
  let open Expr in
  run "filter (amount>500 & cat=c3)"
    (o |> L.filter (col "amount" > float 500. && col "category" = str "c3"));
  run "with_column + sum"
    (o |> L.with_column (col "amount" * float 1.2 + int 1 |> alias "gross")
       |> L.select [ sum (col "gross") |> alias "total" ]);
  run "group_by category (20 groups)"
    (o |> L.group_by [ col "category" ]
       |> L.agg [ sum (col "amount") |> alias "total"; mean (col "amount") |> alias "avg"; len () |> alias "n" ]);
  run "group_by user_id (10k groups)"
    (o |> L.group_by [ col "user_id" ] |> L.agg [ sum (col "amount") |> alias "total" ]);
  run "join users + group by country"
    (o |> L.join ~on:[ "user_id" ] u
       |> L.group_by [ col "country" ]
       |> L.agg [ sum (col "amount") |> alias "revenue" ])

let () =
  (* peak resident memory of this process *)
  try
    let ic = open_in "/proc/self/status" in
    (try
       while true do
         let l = input_line ic in
         if String.length l > 6 && String.sub l 0 6 = "VmHWM:" then print_endline ("peak RSS " ^ String.trim (String.sub l 6 (String.length l - 6)))
       done
     with End_of_file -> close_in ic)
  with _ -> ()
