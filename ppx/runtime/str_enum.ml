(**
  This module defines the String Enumeration kind, which are data
  fields that can only take a specific set of string values.
*)

open! Core_kernel

type spec = { valid_values: String.Set.t } [@@unboxed]

type stats = { distrib: int String.Table.t } [@@unboxed] [@@deriving fields]

let stats_init () = { distrib = String.Table.create () }

(** Updates field stats given a new field value. *)
let stats_update (_ : spec) x stats =
  Fields_of_stats.map ~distrib:(fun field ->
      let distrib = Field.get field stats in
      String.Table.incr distrib x;
      distrib)

type report = {
  num_values: int;
  distrib: int String.Table.t; [@to_yojson Aux.int_tbl_to_yojson]
  num_invalid_values: int;
  prop_invalid_values: float;
}
[@@deriving to_yojson, sexp]

type report_acc = {
  num_values: int;
  num_invalid_values: int;
}

(** Generates a field report based on the given field spec and stats. *)
let report { valid_values } ({ distrib } : stats) : report =
  let { num_values; num_invalid_values } =
    String.Table.fold distrib ~init:{ num_values = 0; num_invalid_values = 0 }
      ~f:(fun ~key ~data { num_values; num_invalid_values } ->
        {
          num_values = num_values + data;
          num_invalid_values =
            (if String.Set.mem valid_values key then num_invalid_values else num_invalid_values + data);
        })
  in
  { num_values; distrib; num_invalid_values; prop_invalid_values = num_invalid_values // num_values }
