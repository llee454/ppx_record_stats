(**
  This module defines the Ranged Integer kind, which are data fields
  that store bounded integral values.
*)

open! Core

type spec = {
  low: int option;
  high: int option;
}

type stats = {
  num_values: int;
  num_invalid_values: int;
  min: int option;
  max: int option;
}

let stats_init () = { num_values = 0; num_invalid_values = 0; min = None; max = None }

let stats_update { low; high } x { num_values; num_invalid_values; min; max } =
  {
    num_values = num_values + 1;
    num_invalid_values =
      num_invalid_values
      + Bool.to_int
          (Option.value_map low ~default:false ~f:(fun low -> x < low)
          || Option.value_map high ~default:false ~f:(fun high -> high < x)
          );
    min = Some (Option.value_map min ~default:x ~f:(fun curr -> Int.min curr x));
    max = Some (Option.value_map max ~default:x ~f:(fun curr -> Int.max curr x));
  }

type report = {
  num_values: int;
  num_invalid_values: int;
  prop_invalid_values: float;
  min: int option;
  max: int option;
}
[@@deriving to_yojson, sexp]

let report (_ : spec) ({ num_values; num_invalid_values; min; max } : stats) : report =
  { num_values; num_invalid_values; prop_invalid_values = num_invalid_values // num_values; min; max }
