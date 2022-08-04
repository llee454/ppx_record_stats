open! Core_kernel
open! Ppxlib
open Ast_builder.Default

type field_spec = {
  field_name: string;
  field_type: core_type;
  specs: expression;
}
[@@deriving fields]

let parse_attrib =
  Attribute.declare "stats" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (function
      | { pexp_desc = Pexp_array _; _ } as spec_exprs -> spec_exprs
      | _ -> failwiths ~here:[%here] "" () [%sexp_of: unit])

let parse_label_decl (ld : label_declaration) =
  Option.(
    Attribute.get parse_attrib ld >>| fun specs ->
    { field_name = ld.pld_name.txt; field_type = ld.pld_type; specs })

let create_rec_decl ~loc txt label_decls =
  type_declaration ~loc ~name:{ txt; loc } ~params:[] ~cstrs:[] ~kind:(Ptype_record label_decls)
    ~private_:Public ~manifest:None

let create_deriving_fields_attrib ~loc =
  attribute ~loc ~name:{ txt = "deriving"; loc } ~payload:(PStr [ [%stri fields] ])

let create_deriving_fields_json_attrib ~loc =
  attribute ~loc ~name:{ txt = "deriving"; loc } ~payload:(PStr [ [%stri fields, to_yojson] ])

let add_deriving_fields_attrib ~loc type_decl =
  { type_decl with ptype_attributes = [ create_deriving_fields_attrib ~loc ] }

let add_deriving_fields_json_attrib ~loc type_decl =
  { type_decl with ptype_attributes = [ create_deriving_fields_json_attrib ~loc ] }

let create_stats_decl ~loc specs =
  pstr_type ~loc Recursive
    [
      (* create record label declarations *)
      List.map specs ~f:(fun { field_name; field_type; _ } ->
          label_declaration ~loc
            ~name:{ txt = sprintf "%s_stats" field_name; loc }
            ~mutable_:Immutable ~type_:[%type: (module Field_stats with type t = [%t field_type]) list])
      (* create record declaration and add deriving fields attribute *)
      |> create_rec_decl ~loc "stats"
      |> add_deriving_fields_attrib ~loc;
    ]

let create_report_decl ~loc specs =
  pstr_type ~loc Recursive
    [
      (* create record label declarations *)
      List.map specs ~f:(fun { field_name; _ } ->
          label_declaration ~loc
            ~name:{ txt = sprintf "%s_rep" field_name; loc }
            ~mutable_:Immutable ~type_:[%type: field_report list])
      (* create record declaration and add deriving fields attribute *)
      |> create_rec_decl ~loc "report"
      |> add_deriving_fields_json_attrib ~loc;
    ]

let create_fn_ident ~loc name =
  {
    pexp_desc = Pexp_ident { txt = Lident name; loc };
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let create_stats_init_def ~loc specs =
  let args =
    List.map specs ~f:(fun { field_name; specs; _ } ->
        ( Labelled (sprintf "%s_stats" field_name),
          [%expr Array.map ~f:field_init [%e specs] |> Array.to_list] ))
  in
  let fn = pexp_apply ~loc [%expr Fields_of_stats.create] args in
  [%stri let stats_init () = [%e fn]]

let create_report_def ~loc specs =
  let update_args =
    List.map specs ~f:(fun { field_name; _ } ->
        let fn = create_fn_ident ~loc field_name in
        Labelled (sprintf "%s_stats" field_name), [%expr stats_update stats ([%e fn] x)])
  in
  let report_args =
    List.map specs ~f:(fun { field_name; _ } ->
        let fn = create_fn_ident ~loc (sprintf "%s_stats" field_name) in
        Labelled (sprintf "%s_rep" field_name), [%expr [%e fn] stats |> stats_report])
  in
  let update_fn = pexp_apply ~loc [%expr Fields_of_stats.iter] update_args in
  let report_fn = pexp_apply ~loc [%expr Fields_of_report.create] report_args in
  [%stri
    let report xs =
      let stats = stats_init () in
      List.iter xs ~f:(fun x -> [%e update_fn]);
      [%e report_fn]]

let expand_type_decl ~loc = function
| { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _ } ->
  let ext = Location.raise_errorf ~loc:ptype_loc "Cannot derive accessors for non record types" in
  [ pstr_extension ~loc ext [] ]
| { ptype_kind = Ptype_record fields; _ } ->
  let specs = List.filter_map fields ~f:parse_label_decl in
  [
    create_stats_decl ~loc specs;
    create_report_decl ~loc specs;
    create_stats_init_def ~loc specs;
    create_report_def ~loc specs;
  ]

let create_stats_mod ~loc items =
  {
    pstr_desc =
      Pstr_module
        (module_binding ~loc ~name:{ txt = Some "Stats"; loc }
           ~expr:
             {
               pmod_desc = Pmod_structure ([%stri include Runtime] :: items);
               pmod_loc = loc;
               pmod_attributes = [];
             });
    pstr_loc = loc;
  }

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [ List.concat_map type_declarations ~f:(expand_type_decl ~loc) |> create_stats_mod ~loc ]

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) : signature_item list = []

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let ppx_validation =
  Deriving.add "record_stats" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
