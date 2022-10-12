open! Core

let int_tbl_to_yojson xs = String.Table.to_alist xs |> [%to_yojson: (string * int) list]
let num_tbl_to_yojson xs = Int.Table.to_alist xs |> [%to_yojson: (int * int) list]

let num_tbl_opt_to_yojson xs =
  Option.map xs ~f:Int.Table.to_alist |> [%to_yojson: (int * int) list option]

let date_to_yojson x = Date.to_string_iso8601_basic x |> [%to_yojson: string]
let date_opt_to_yojson x = Option.map x ~f:Date.to_string_iso8601_basic |> [%to_yojson: string option]
