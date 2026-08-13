type t = Scan of Row.t list * int | Filter of t * Expr.t

let scan data ~chunk_size = Scan (data, chunk_size)
let filter child e = Filter (child, e)

let chunk_rows (rows : Row.t list) (chunk_size : int) : Row.t list Seq.t =
  let rec go rows () =
    match rows with
    | [] -> Seq.Nil
    | _ ->
        let rec take n acc rest =
          match (n, rest) with
          | 0, _ | _, [] -> (List.rev acc, rest)
          | n, x :: xs -> take (n - 1) (x :: acc) xs
        in
        let chunk, rest = take chunk_size [] rows in
        Seq.Cons (chunk, go rest)
  in
  go rows

let rec execute (p : t) : Batch.t Seq.t =
  match p with
  | Scan (data, chunk_size) ->
      chunk_rows data chunk_size |> Seq.map Batch.of_rows
  | Filter (child, e) ->
      execute child
      |> Seq.map (fun batch ->
             let mask = Expr.eval_mask batch e in
             Batch.apply_mask batch mask)
