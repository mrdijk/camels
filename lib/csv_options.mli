(** Options that describe how to parse a CSV file. Shared by the CSV engine and
    by plans that scan CSV files. *)

type t = {
  sep : char;  (** field separator *)
  quote : char;  (** quote character *)
  has_header : bool;  (** the first record holds the column names *)
  null_values : string list;
      (** unquoted fields equal to one of these are null *)
}
