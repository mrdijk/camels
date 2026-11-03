open Camels
module L = Lazyframe

let ( let* ) = Result.bind

type order = {
  id : int;
  region : string;
  amount : float;
}

let print_result (rows : Row.t list) : unit =
  List.iter
    (fun (row : Row.t) ->
      Array.iter
        (fun v -> Printf.printf "%s " (Option.fold ~none:"null" ~some:Value.to_string v))
        row;
      print_newline ())
    rows

 let pipeline =
  let open Expr in
  Lazyframe.read_csv_lazy "btcusd_1-min_data.csv"
  |> fun df -> L.filter df ( col "Volume" >. F 0.0 )

let employees =
  Dataframe.create
    [
      Series.of_ints "id" [| 1; 2; 3; 4; 5; 6 |];
      Series.of_strings "name" [| "Alice"; "Bob"; "Cara"; "Dan"; "Eve"; "Fay" |];
      Series.of_opt_ints "dept_id" [| Some 10; Some 10; Some 20; Some 20; Some 30; None |];
      Series.of_floats "salary" [| 100.; 200.; 150.; 50.; 300.; 80. |];
    ]

let departments =
  Dataframe.create
    [
      Series.of_ints "dept_id" [| 10; 20; 40 |];
      Series.of_strings "dept_name" [| "Eng"; "Ops"; "HR" |];
      Series.of_strings "region" [| "EU"; "US"; "EU" |];
    ]

let () =
  let open Expr in
  let query =
    L.of_df employees
    |> L.join ~on:[ "dept_id" ] (L.of_df departments)                  (* join       *)
    |> L.filter (col "region" = str "EU" && col "salary" > float 60.)  (* filter     *)
    |> L.with_column (col "salary" * float 1.1 |> alias "raised")      (* with column *)
    |> L.group_by [ col "dept_name" ]                                  (* aggregate  *)
    |> L.agg
         [ len () |> alias "headcount";
           sum (col "raised") |> alias "payroll";
           max (col "salary") |> alias "top_salary" ]
    |> L.select [ col "dept_name"; col "headcount"; col "payroll" ]    (* project    *)
  in
  print_endline "=== naive plan ===";
  print_string (L.explain ~optimize:false query);
  print_endline "\n=== optimised plan ===";
  print_string (L.explain query);
  print_endline "\n=== result ===";
  Dataframe.print (L.collect query)
