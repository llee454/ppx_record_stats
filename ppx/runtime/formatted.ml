(**
  This module defines the Formatted String kind, which are data fields
  that contain string values that must conform to a specific format.
*)

open! Core_kernel

type spec = { format: Re.re } [@@unboxed]

type stats = {
  num_values: int;
  num_invalid_values: int;
}
[@@deriving fields]

let stats_init () = { num_values = 0; num_invalid_values = 0 }

let stats_update { format } x { num_values; num_invalid_values } =
  {
    num_values = num_values + 1;
    num_invalid_values = (if Re.execp format x then num_invalid_values else num_invalid_values + 1);
  }

type report = {
  num_values: int;
  num_invalid_values: int;
  prop_invalid_values: float;
}
[@@deriving to_yojson, sexp]

let report (_ : spec) ({ num_values; num_invalid_values } : stats) : report =
  { num_values; num_invalid_values; prop_invalid_values = num_invalid_values // num_values }
