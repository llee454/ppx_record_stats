open! Core_kernel

type ('spec, 'transform) field_spec_params = {
  spec: 'spec;
  transform: 'transform;
}

type 'a field_spec =
  | Date_ranged of (Date_ranged.spec, 'a -> Date.t list) field_spec_params
  | Formatted   of (Formatted.spec, 'a -> string list) field_spec_params
  | Int_ranged  of (Int_ranged.spec, 'a -> int list) field_spec_params
  | Linked      of ('a Linked.spec, 'a -> 'a list) field_spec_params
  | Nullable    of (Nullable.spec, 'a -> unit option) field_spec_params
  | Str_enum    of (Str_enum.spec, 'a -> string list) field_spec_params

type field_report =
  | Date_ranged_rep of Date_ranged.report
  | Formatted_rep   of Formatted.report
  | Int_ranged_rep  of Int_ranged.report
  | Linked_rep      of Linked.report
  | Nullable_rep    of Nullable.report
  | Str_enum_rep    of Str_enum.report
[@@deriving to_yojson, sexp]

module type Field_stats = sig
  (** field value type *)
  type t

  type stats_t

  val stats : stats_t ref

  (** updates the current field statistics based on the current field value *)
  val update : t -> unit

  val report : unit -> field_report
end

let field_init : type a. a field_spec -> (module Field_stats with type t = a) = function
| Date_ranged { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Date_ranged.stats

    let stats = ref (Date_ranged.stats_init ())

    let update field_value =
      transform field_value |> List.iter ~f:(fun x -> stats := Date_ranged.stats_update spec x !stats)

    let report () = Date_ranged_rep (Date_ranged.report spec !stats)
  end
  )
| Formatted { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Formatted.stats

    let stats = ref (Formatted.stats_init ())

    let update field_value =
      transform field_value |> List.iter ~f:(fun x -> stats := Formatted.stats_update spec x !stats)

    let report () = Formatted_rep (Formatted.report spec !stats)
  end
  )
| Int_ranged { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Int_ranged.stats

    let stats = ref (Int_ranged.stats_init ())

    let update field_value =
      transform field_value |> List.iter ~f:(fun x -> stats := Int_ranged.stats_update spec x !stats)

    let report () = Int_ranged_rep (Int_ranged.report spec !stats)
  end
  )
| Linked { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Linked.stats

    let stats = ref (Linked.stats_init ())

    let update field_value =
      transform field_value |> List.iter ~f:(fun x -> stats := Linked.stats_update spec x !stats)

    let report () = Linked_rep (Linked.report spec !stats)
  end
  )
| Nullable { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Nullable.stats

    let stats = ref (Nullable.stats_init ())

    let update field_value = stats := Nullable.stats_update spec (transform field_value) !stats

    let report () = Nullable_rep (Nullable.report spec !stats)
  end
  )
| Str_enum { spec; transform } ->
  ( module struct
    type t = a

    type stats_t = Str_enum.stats

    let stats = ref (Str_enum.stats_init ())

    let update field_value =
      transform field_value |> List.iter ~f:(fun x -> stats := Str_enum.stats_update spec x !stats)

    let report () = Str_enum_rep (Str_enum.report spec !stats)
  end
  )

let stats_update :
   type a.
   'rec_stats ->
   a ->
   ('perms, 'rec_stats, (module Field_stats with type t = a) list) Field.t_with_perm ->
   unit =
 fun rec_stats field_value field ->
  Field.get field rec_stats
  |> List.iter ~f:(fun stats ->
         let module Stats : Field_stats with type t = a = (val stats) in
         Stats.update field_value)

let stats_report : type a. (module Field_stats with type t = a) list -> field_report list =
  List.map ~f:(fun (field_stats : (module Field_stats with type t = a)) ->
      let module M : Field_stats = (val field_stats) in
      M.report ())
