Record Stats PPX
================

The Record Stats PPX is designed to make it easy to statistically analyze datasets. Given a record type, `t`, you can annotate `t`s fields, and then call a single function to generate a report that describes the proportion of field values that are null, the number of field string values that are invalid, etc.

Example
-------

For example, given the record type, `example`, the following annotations tell the library that that `example`s `a` field is an int that should be less than (or equal to) 100; `b` is an optional string; `c` is a date that should be a date that should fall within the range 01/10/2022 and 09/20/2022.

```ocaml
type example = {
  a: int; [@stats [| Int_ranged { spec = { low = None; high = Some 100 }; transform = Option.some } |]]
  b: string option;
      [@stats
        [|
          Str_enum { spec = { valid_values = String.Set.empty }; transform = Fn.id };
          Nullable { spec = (); transform = Option.map ~f:(Fn.const ()) };
        |]]
  c: Date.t;
      [@stats
        [|
          Date_ranged
            {
              spec =
                {
                  start_date = Some (Date.create_exn ~y:2022 ~m:Month.jan ~d:10);
                  end_date = Some (Date.create_exn ~y:2022 ~m:Month.sep ~d:20);
                  granularity = Weeks;
                };
              transform = Option.some;
            };
        |]]
  d: string;
      [@stats
        [|
          Formatted
            {
              spec = { format = Re.Perl.compile_pat {re|numerical [[:digit:]]{5}|re} };
              transform = Option.some;
            };
        |]]
}
[@@deriving fields, record_stats]
```

Given a list of `t` values, `xs`, we can generate a statistics report for `xs` by simply calling `Stats.report xs`. For example:

```ocaml
let xs =
  [
    { a = 0; b = Some "hello"; c = Date.create_exn ~y:2022 ~m:Month.jul ~d:5; d = "numerical 12345" };
    { a = 1; b = Some "world"; c = Date.create_exn ~y:2022 ~m:Month.aug ~d:15; d = "numerical 67890" };
    { a = 2; b = None; c = Date.create_exn ~y:2020 ~m:Month.sep ~d:30; d = "number" };
  ]

let () =
  Stats.report xs
  |> [%to_yojson: Stats.report]
  |> sprintf !"%{Yojson.Safe.pretty_to_string}"
  |> print_endline
```

The resulting report will list information about the values of the fields in `xs`:

```json
{
  "a_rep": [
    [
      "Int_ranged_rep",
      {
        "num_values": 3,
        "num_invalid_values": 0,
        "prop_invalid_values": 0.0,
        "min": 0,
        "max": 2
      }
    ]
  ],
  "b_rep": [
    [
      "Str_enum_rep",
      {
        "num_values": 2,
        "distrib": [ [ "world", 1 ], [ "hello", 1 ] ],
        "num_invalid_values": 2,
        "prop_invalid_values": 1.0
      }
    ],
    [
      "Nullable_rep",
      { "num_values": 3, "num_null": 1, "prop_null": 0.3333333333333333 }
    ]
  ],
  "c_rep": [
    [
      "Date_ranged_rep",
      {
        "num_values": 3,
        "num_invalid_values": 1,
        "prop_invalid_values": 0.3333333333333333,
        "earliest": "20200930",
        "latest": "20220815",
        "distrib": [ [ 31, 1 ], [ -66, 1 ], [ 25, 1 ] ]
      }
    ]
  ],
  "d_rep": [
    [
      "Formatted_rep",
      {
        "num_values": 3,
        "num_invalid_values": 1,
        "prop_invalid_values": 0.3333333333333333
      }
    ]
  ]
}
```

Usage
-----

This library defines a variety of field types. These are listed below:

| Sort        | Field type | Spec type |
| ----------- | ---------- | --------- |
| Date_ranged | Date.t     | { start_date: Date.t option; end_date: Date.t option; granularity = Weeks|Months|Years } |
| Formatted   | string     | { format: Re.re } |
| Int_ranged  | int        | { low: int option; high: int option } |
| Linked      | 'a         | { get_num_matches : 'a -> int } |
| Nullable    | 'a option  | unit |
| Str_enum    | string     | { valid_values: String.Set.t } |

Each field annotation includes a record with three fields: `SORT { spec; transform }` where `SORT` is one of the constructors listed above and `spec` depends on the sort.

For example:

```ocaml
[@stats
  [|
    Str_enum {
      spec = { valid_values = String.Set.of_list [ "true"; "false" ] };
      transform = Fn.id;
    }
  |]
]
```

is a valid field annotation.

You must attach the PPX annotation to each type definition you are trying to analyze.

This library will create a module named `Stats` in the same context as the type definition. To prevent this module from conflicting with other definitions, you may need to enclose the type definition in its own module.

To generate a report, call `Stats.report xs` where `xs` denotes the dataset and has type `xs : t list`.

Usage in Other Projects
-----------------------

To use this PPX library in another project include the `ppx_record_stats` library in your Dune file's preprocess section. For example:

```
(executable
 (name main)
 (libraries
  core_kernel
  )
 (preprocess
  (pps ppx_jane ppx_record_stats))
  (modes exe)
)
```

You must also update your OPAM package configuration file. Add the following line to your "pin-depends" section: `["ppx_record_stats.0.1.0" "git+https://github.com/llee454/ppx_record_stats.git#main"]`. Add the following to your "depends" section: `  "ppx_record_stats" { = "0.1.0"}`.


Initializing the Build Environment
----------------------------------

Note: the following are a hack solution to fix the fact that brew and opam are
out of sync and opam's owl-plplot library requires library versions that can no
longer be installed with brew.

```
opam switch create . ocaml-variants.4.10.0+flambda --no-install
opam update
opam install --deps-only .
dune build
```

Known Bugs
----------

1. The types defined by the module generated by this PPX are not compatible with the -unboxed-types Dune build flag when the target record only has a single annotated field.