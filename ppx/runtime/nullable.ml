(**
  This module defines the Nullable kind, which are data fields that
  can store null values.
*)

open! Core_kernel

type spec = unit

type stats = {
  num_values: int;
  num_null: int;
}
[@@deriving fields]

let stats_init () = { num_values = 0; num_null = 0 }

(** Updates field stats given a new field value *)
let stats_update (() : spec) x { num_values; num_null } =
  { num_values = num_values + 1; num_null = (if Option.is_some x then num_null else num_null + 1) }

type report = {
  num_values: int;
  num_null: int;
  prop_null: float;
}
[@@deriving to_yojson, sexp]

let report (() : spec) ({ num_values; num_null } : stats) : report =
  { num_values; num_null; prop_null = num_null // num_values }
