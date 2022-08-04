(**
  This module defines the Linked kind, which are data fields that
  contain values that are used to cross reference entries between
  datasets.
*)

open! Core_kernel

type 'a spec = { get_num_matches: 'a -> int } [@@unboxed]

type stats = {
  (* the number of links that have k matches in the other dataset. *)
  distrib: int Int.Table.t;
}
[@@unboxed] [@@deriving fields]

let stats_init () = { distrib = Int.Table.create () }

let stats_update { get_num_matches } x { distrib } =
  let () = Int.Table.incr distrib (get_num_matches x) in
  { distrib }

type report = {
  num_unmatched: int;
  num_uniq_match: int;
  num_mult_matches: int;
  distrib: int Int.Table.t; [@to_yojson Aux.num_tbl_to_yojson]
}
[@@deriving to_yojson, sexp]

let report (_ : 'a spec) ({ distrib } : stats) : report =
  {
    num_unmatched = Int.Table.find distrib 0 |> Option.value ~default:0;
    num_uniq_match = Int.Table.find distrib 1 |> Option.value ~default:0;
    num_mult_matches =
      Int.Table.fold distrib ~init:0 ~f:(fun ~key ~data acc -> if key > 1 then acc + data else acc);
    distrib;
  }
