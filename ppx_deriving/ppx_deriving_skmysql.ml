open Ppxlib

module Type_repr = struct
  type category =
    | Char of int
    | Varchar of int
    | Binary of int
    | Blob
    | Tiny_int
    | Small_int
    | Medium_int
    | Int
    | Big_int
    | Float
    | Datetime
    | Date
    | Time

  type t = category * expression option

  (* CR-someday orau: Maybe there's a better way to generate these so that we
     don't have to manually give each type an associated string? *)
  let sized_types =
    [
      ("char", fun size -> Char size);
      ("varchar", fun size -> Varchar size);
      ("binary", fun size -> Binary size);
    ]

  let unsized_types =
    [
      ("blob", Blob);
      ("tiny_int", Tiny_int);
      ("small_int", Small_int);
      ("medium_int", Medium_int);
      ("int", Int);
      ("big_int", Big_int);
      ("float", Float);
      ("datetime", Datetime);
      ("date", Date);
      ("time", Time);
    ]

  let is_sized_type = function
    | { pexp_desc = Pexp_ident { txt = lident; _ }; _ } ->
      let name = Longident.name lident in
      List.mem_assoc name sized_types
    | _ -> false

  let get_size ~loc = function
    | { pexp_desc = Pexp_constant (Pconst_integer (length, _)); _ } ->
      int_of_string length
    | _ ->
      Location.raise_errorf ~loc
        "Couldn't parse length in [@repr ...] attribute"

  let get_sized_type ~loc ~typename ~length =
    let length = get_size ~loc length in
    match typename with
    | { pexp_desc = Pexp_ident { txt = lident; _ }; _ } ->
      let name = Longident.name lident in
      let construct = List.assoc name sized_types in
      construct length
    | _ ->
      Location.raise_errorf ~loc "Unknown sized type in [@repr ...] attribute"

  let get_unsized_type ~loc ~typename =
    match typename with
    | { pexp_desc = Pexp_ident { txt = lident; _ }; _ } ->
      let name = Longident.name lident in
      List.assoc name unsized_types
    | _ -> Location.raise_errorf ~loc "Unknown type in [@repr ...] attribute"

  let of_repr ~loc = function
    | [%expr [%e? typename] [%e? length]] when is_sized_type typename ->
      (get_sized_type ~loc ~typename ~length, None)
    | [%expr [%e? typename] [%e? length] [%e? conv]] when is_sized_type typename
      ->
      (get_sized_type ~loc ~typename ~length, Some conv)
    | [%expr [%e? typename] [%e? conv]] ->
      (get_unsized_type ~loc ~typename, Some conv)
    | typename -> (get_unsized_type ~loc ~typename, None)

  let of_core_type ~loc = function
    (* CR orau: Should [string] always default to [blob]? Or should we require a
       manual [[@repr ...]]? *)
    | [%type: string] -> (Blob, None)
    | [%type: int] -> (Int, None)
    | [%type: int64] -> (Big_int, None)
    | [%type: float] -> (Float, None)
    | [%type: Datetime.t] -> (Datetime, None)
    | [%type: Date.t] -> (Date, None)
    | [%type: Time.t] -> (Time, None)
    | _ ->
      Location.raise_errorf ~loc
        "Couldn't infer column type; try providing a [@repr ...] attribute"
end

(* CR-someday orau: This also has way too much code repeated. Maybe we can just
   replace [Type_repr.t] with a record that stores function name, string
   representation, etc. & just look up the corresponding entry for each type? *)
let expr_of_column
    ~loc
    ~column_name
    ~repr:(category, conv)
    ~auto_increment
    ~nullable =
  let column_name = Ast_builder.Default.estring ~loc column_name in
  let nullable = Ast_builder.Default.ebool ~loc nullable in
  let auto_increment = Ast_builder.Default.ebool ~loc auto_increment in
  let expr_of_size size = Ast_builder.Default.eint ~loc size in
  let conv = Option.value ~default:[%expr Ezmysql.Column.Conv.identity] conv in
  match category with
  | Type_repr.Char size ->
    [%expr
      Ezmysql.Column.make_char [%e column_name] [%e expr_of_size size]
        ~nullable:[%e nullable] [%e conv]]
  | Varchar size ->
    [%expr
      Ezmysql.Column.make_varchar [%e column_name] [%e expr_of_size size]
        ~nullable:[%e nullable] [%e conv]]
  | Binary size ->
    [%expr
      Ezmysql.Column.make_binary [%e column_name] [%e expr_of_size size]
        ~nullable:[%e nullable] [%e conv]]
  | Blob ->
    [%expr
      Ezmysql.Column.make_blob [%e column_name] ~nullable:[%e nullable]
        [%e conv]]
  | Tiny_int ->
    [%expr
      Ezmysql.Column.make_tiny_int [%e column_name] ~nullable:[%e nullable]
        ~auto_increment:[%e auto_increment] [%e conv]]
  | Small_int ->
    [%expr
      Ezmysql.Column.make_small_int [%e column_name] ~nullable:[%e nullable]
        ~auto_increment:[%e auto_increment] [%e conv]]
  | Medium_int ->
    [%expr
      Ezmysql.Column.make_medium_int [%e column_name] ~nullable:[%e nullable]
        ~auto_increment:[%e auto_increment] [%e conv]]
  | Int ->
    [%expr
      Ezmysql.Column.make_int [%e column_name] ~nullable:[%e nullable]
        ~auto_increment:[%e auto_increment] [%e conv]]
  | Big_int ->
    [%expr
      Ezmysql.Column.make_big_int [%e column_name] ~nullable:[%e nullable]
        ~auto_increment:[%e auto_increment] [%e conv]]
  | Float ->
    [%expr
      Ezmysql.Column.make_float [%e column_name] ~nullable:[%e nullable]
        [%e conv]]
  | Datetime ->
    [%expr
      Ezmysql.Column.make_datetime [%e column_name] ~nullable:[%e nullable]
        [%e conv]]
  | Date ->
    [%expr
      Ezmysql.Column.make_date [%e column_name] ~nullable:[%e nullable]
        [%e conv]]
  | Time ->
    [%expr
      Ezmysql.Column.make_date [%e column_name] ~nullable:[%e nullable]
        [%e conv]]

let generate_column ~loc field =
  let auto_increment =
    Attribute.get Attrs.auto_increment field |> Option.is_some
  and nullable = Attribute.get Attrs.nullable field |> Option.is_some
  and column_name =
    Attribute.get Attrs.column_name field
    |> Option.value ~default:field.pld_name.txt
  and repr =
    match Attribute.get Attrs.repr field with
    | Some repr -> Type_repr.of_repr ~loc repr
    | None -> Type_repr.of_core_type ~loc field.pld_type
  in
  let rhs = expr_of_column ~loc ~column_name ~repr ~auto_increment ~nullable in
  Ast_builder.Default.(
    pstr_value ~loc Nonrecursive
      [ value_binding ~loc ~pat:(ppat_var ~loc field.pld_name) ~expr:rhs ]
  )

let generate_column_intf ~loc field =
  Ast_builder.Default.(
    psig_value ~loc
      (value_description ~loc ~name:field.pld_name ~type_:field.pld_type
         ~prim:[]
      )
  )

let const_list_fold list ~loc ~f =
  let empty_list =
    Ast_builder.Default.pexp_construct ~loc
      { txt = Longident.Lident "[]"; loc }
      None
  in
  let cons hd tl =
    Ast_builder.Default.(
      pexp_construct ~loc
        { txt = Longident.Lident "::"; loc }
        (Some (pexp_tuple ~loc [ hd; tl ]))
    )
  in
  List.fold_left (fun accum item -> cons (f item) accum) empty_list list

let generate_table ~loc tydecl =
  let fields =
    match tydecl.ptype_kind with
    | Ptype_record fields -> fields
    | _ ->
      Location.raise_errorf ~loc "Expected record type for [@@deriving ezmysql]"
  in
  let pkey =
    let pkey_column =
      List.find_map
        (fun field ->
          Option.map
            (fun _ -> field.pld_name)
            (Attribute.get Attrs.primary_key field)
        )
        fields
    in
    match pkey_column with
    | None ->
      Location.raise_errorf ~loc
        "[@@deriving ezmysql] requires a [@primary_key] column"
    | Some pkey_column -> pkey_column
  in
  let pkey_exp = Ast_builder.Default.evar ~loc:pkey.loc pkey.txt in
  let table_name =
    match Attribute.get Attrs.table_name tydecl with
    | None ->
      Location.raise_errorf ~loc
        "[@@deriving ezmysql] requires a [@@table_name ...] annotation"
    | Some name -> name
  in
  let table_name_exp = Ast_builder.Default.estring ~loc table_name in
  let columns_exp =
    const_list_fold fields ~loc ~f:(fun field ->
        let field_exp = Ast_builder.Default.evar ~loc field.pld_name.txt in
        [%expr Pack [%e field_exp]]
    )
  in
  let column_decls = List.map (generate_column ~loc) fields in
  let pack_rows_exp =
    const_list_fold fields ~loc ~f:(fun field ->
        let field_ident = Ast_builder.Default.evar ~loc field.pld_name.txt in
        let field_value =
          Ast_builder.Default.pexp_field ~loc [%expr t]
            { loc; txt = Longident.Lident field.pld_name.txt }
        in
        let nullable = Attribute.get Attrs.nullable field |> Option.is_some in
        let pack_function =
          if nullable then
            [%expr Ezmysql.pack_column_opt]
          else
            [%expr Ezmysql.pack_column]
        in
        [%expr [%e pack_function] [%e field_ident] [%e field_value]]
    )
  in
  let unpack_rows_exp =
    let unpacked_rows =
      List.map
        (fun field ->
          let key = { txt = Longident.Lident field.pld_name.txt; loc } in
          let value = Ast_builder.Default.evar ~loc field.pld_name.txt in
          let nullable = Attribute.get Attrs.nullable field |> Option.is_some in
          let unpack_function =
            if nullable then
              [%expr
                fun spec row ->
                  Ezmysql.find_column spec row |> Rresult.R.failwith_error_msg]
            else
              [%expr Ezmysql.get_column]
          in
          (key, [%expr [%e unpack_function] [%e value] row])
        )
        fields
    in
    Ast_builder.Default.(pexp_record ~loc unpacked_rows None)
  in
  column_decls
  @ [%str
      let table =
        Table.make
          ~primary_key:[ Pack [%e pkey_exp] ]
          [%e table_name_exp] [%e columns_exp]

      let to_row t = row_of_list [%e pack_rows_exp]

      let of_row row =
        try Ok [%e unpack_rows_exp] with
        | _ -> Rresult.R.error_msgf "Invalid %a row" Pp.table_name table]

let generate_table_intf ~loc tydecl =
  let fields =
    match tydecl.ptype_kind with
    | Ptype_record fields -> fields
    | _ ->
      Location.raise_errorf ~loc "Expected record type for [@@deriving ezmysql]"
  in
  List.map (generate_column_intf ~loc) fields

let generate_impl ~ctxt:_ (_rec_flag, type_declarations) =
  List.concat_map
    (fun ({ ptype_name = { txt; loc }; _ } as tydecl) ->
      if txt = "t" then
        generate_table ~loc tydecl
      else
        []
    )
    type_declarations

let generate_intf ~ctxt:_ (_rec_flag, type_declarations) =
  List.concat_map
    (fun ({ ptype_name = { txt; loc }; _ } as tydecl) ->
      if txt = "t" then
        [%sig:
          val to_row : t -> row
          val of_row : row -> (t, [> `Msg of string ]) result
          val table : Table.t]
        @ generate_table_intf ~loc tydecl
      else
        []
    )
    type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriving_ezmysql =
  Deriving.add "ezmysql" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
