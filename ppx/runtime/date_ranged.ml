(**
  This module defines the Ranged Date kind, which are data fields that
  store date values that should fall within a specified range.
*)

open! Core

type granularity =
  | Weeks
  | Months
  | Years  of int

type spec = {
  start_date: Date.t option;
  end_date: Date.t option;
  granularity: granularity;
}

type stats = {
  num_values: int;
  num_invalid_values: int;
  earliest: Date.t option;
  latest: Date.t option;
  dates: Date.t list;
}

let stats_init () = { num_values = 0; num_invalid_values = 0; earliest = None; latest = None; dates = [] }

let stats_update { start_date; end_date; granularity = _ } x
   { num_values; num_invalid_values; earliest; latest; dates } =
  {
    num_values = num_values + 1;
    num_invalid_values =
      (num_invalid_values
      +
      match start_date, end_date with
      | Some low, Some high -> Bool.to_int (not @@ Date.between x ~low ~high)
      | Some low, _ -> Bool.to_int Date.(x < low)
      | _, Some high -> Bool.to_int Date.(high < x)
      | _ -> 0
      );
    earliest =
      Some (Option.value_map earliest ~default:x ~f:(fun curr -> if Date.(x < curr) then x else curr));
    latest = Some (Option.value_map latest ~default:x ~f:(fun curr -> if Date.(x > curr) then x else curr));
    dates = x :: dates;
  }

type report = {
  num_values: int;
  num_invalid_values: int;
  prop_invalid_values: float;
  earliest: Date.t option; [@to_yojson Aux.date_opt_to_yojson]
  latest: Date.t option; [@to_yojson Aux.date_opt_to_yojson]
  distrib: int Int.Table.t option; [@to_yojson Aux.num_tbl_opt_to_yojson]
}
[@@deriving to_yojson, sexp]

let report ({ start_date; end_date = _; granularity } : spec)
   ({ num_values; num_invalid_values; earliest; latest; dates } : stats) : report =
  let low =
    match start_date, earliest with
    | Some low, _ -> Some low
    | _, Some low -> Some low
    | _, _ -> None
  in
  let distrib =
    Option.map low ~f:(fun low ->
        List.fold dates ~init:(Int.Table.create ()) ~f:(fun acc x ->
            Int.Table.incr acc
              (match granularity with
              | Weeks -> Date.diff x low / 7
              | Months ->
                (12 * (Date.year x - Date.year low))
                + (Month.to_int (Date.month x) - Month.to_int (Date.month low))
              | Years n -> (Date.year x - Date.year low) / n);
            acc
        )
    )
  in
  {
    num_values;
    num_invalid_values;
    prop_invalid_values = num_invalid_values // num_values;
    earliest;
    latest;
    distrib;
  }
