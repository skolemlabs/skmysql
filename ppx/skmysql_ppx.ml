open! Ppxlib

module String = struct
  include String

  let substr_index ?(pos = 0) t ~pattern =
    let index = ref None in
    for i = pos to length t - length pattern do
      let pattern_index = ref 0 in
      while
        !pattern_index < length pattern
        && t.[i + !pattern_index] = pattern.[!pattern_index]
      do
        pattern_index := !pattern_index + 1
      done;
      if !index = None && !pattern_index = length pattern then index := Some i
    done;
    !index

  let slice t start stop =
    if stop = 0 && start > 0 then
      sub t start (length t - start)
    else
      sub t start (stop - start)
end

module Query_text = struct
  type t =
    | String of string
    | Interpreted of string

  let quote_start = "${"
  let quote_end = "}"

  let rec of_string ~loc str =
    let first_quote_start = String.substr_index str ~pattern:quote_start in
    let first_quote_end =
      String.substr_index ?pos:first_quote_start str ~pattern:quote_end
    in
    match (first_quote_start, first_quote_end) with
    | (Some start_idx, Some end_idx) ->
      let prefix = String.slice str 0 start_idx
      and quoted =
        String.slice str (start_idx + String.length quote_start) end_idx
      and postfix = String.slice str (end_idx + String.length quote_end) 0 in
      String prefix :: Interpreted quoted :: of_string ~loc postfix
    | (Some _, None) ->
      Location.raise_errorf ~loc
        "Unterminated '${' in ezmysql query string, expected '}'!"
    | (None, Some _) ->
      Location.raise_errorf ~loc
        "Encountered unmatched '}' in ezmysql query string, expected '${'!"
    | (None, None) -> [ String str ]
end

module Query = struct
  type t =
    | String of string
    | Column_name of Longident.t Asttypes.loc
    | Table_name of Longident.t Asttypes.loc
    | Custom of expression * expression

  let parse_expression ~loc string =
    let lexbuf = Lexing.from_string string in
    lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
    lexbuf.lex_curr_p <- loc.loc_start;
    match Parse.expression lexbuf with
    | exception _ ->
      Location.raise_errorf ~loc "Could not parse expression: %S" string
    | { pexp_desc = Pexp_ident column; _ } -> Column_name column
    | { pexp_desc = Pexp_construct (table, None); _ } -> Table_name table
    | [%expr [%e? printer], [%e? value]] -> Custom (printer, value)
    | _ ->
      Location.raise_errorf ~loc
        "Expected column name, table name, or custom printer; got: %S" string

  let rec of_query_text ~loc = function
    | Query_text.String str :: rest -> String str :: of_query_text ~loc rest
    | Interpreted exp :: rest ->
      parse_expression ~loc exp :: of_query_text ~loc rest
    | [] -> []

  let rec to_format_string = function
    | String str :: rest -> str ^ to_format_string rest
    | Column_name _ :: rest -> "%a.%a" ^ to_format_string rest
    | Table_name _ :: rest -> "%a" ^ to_format_string rest
    | Custom _ :: rest -> "%a" ^ to_format_string rest
    | [] -> ""

  let expr_of_ident ~loc ident =
    {
      pexp_desc = Pexp_ident { loc; txt = ident };
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }

  let rec to_format_args = function
    | String _ :: rest -> to_format_args rest
    | Table_name { txt = table_name; loc } :: rest ->
      [%expr Ezmysql.Pp.table_name]
      :: expr_of_ident ~loc (Longident.Ldot (table_name, "table"))
      :: to_format_args rest
    | Column_name { txt = Longident.Ldot (table_name, _) as column_name; loc }
      :: rest ->
      [%expr Ezmysql.Pp.table_name]
      :: expr_of_ident ~loc (Longident.Ldot (table_name, "table"))
      :: [%expr Ezmysql.Pp.spec_name]
      :: expr_of_ident ~loc column_name
      :: to_format_args rest
    | Column_name { txt; loc } :: _ ->
      let name = Longident.name txt in
      Location.raise_errorf ~loc
        "Expected qualified reference to column within table module, but got \
         '%s'"
        name
    | Custom (printer, value) :: rest ->
      let loc = printer.pexp_loc in
      [%expr [%e printer] [%e value]] :: to_format_args rest
    | [] -> []
end

let rec extract_args ~loc accum = function
  | [
      (Nolabel, { pexp_desc = Pexp_constant (Pconst_string (payload, _, _)); _ });
    ] ->
    (List.rev accum, payload)
  | arg :: args -> extract_args ~loc (arg :: accum) args
  | _ ->
    Location.raise_errorf ~loc
      "Expected function application ending with query string for [%%ezmysql]"

let extract_payload ~loc = function
  | { pexp_desc = Pexp_apply (fn, args); _ } ->
    let last_index = List.length args - 1 in
    let last_arg_opt = List.nth_opt args last_index
    and other_args = List.filteri (fun i _ -> i <> last_index) args in
    ( match last_arg_opt with
    | Some
        ( Nolabel,
          { pexp_desc = Pexp_constant (Pconst_string (payload, _, _)); _ }
        ) ->
      (fn, other_args, payload)
    | _ ->
      Location.raise_errorf ~loc
        "Expected last argument of function in [%%ezmysql] to be a format \
         string"
    )
  | _ ->
    Location.raise_errorf ~loc
      "Expected function application ending with payload string"

let query_expand ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (fn, other_args, payload) = extract_payload ~loc expr in
  let query_text = Query_text.of_string ~loc payload in
  let query = Query.of_query_text ~loc query_text in
  let format_string = Query.to_format_string query in
  let format_args =
    Query.to_format_args query |> List.map (fun arg -> (Nolabel, arg))
  in
  let format_string_expr = Ast_builder.Default.estring ~loc format_string in
  [%expr
    [%e
      Ast_builder.Default.pexp_apply ~loc fn
        (other_args @ ((Nolabel, format_string_expr) :: format_args))]]

let query_extension =
  Extension.V3.declare "ezmysql" Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    query_expand

let query_rule = Context_free.Rule.extension query_extension
let () = Driver.register_transformation ~rules:[ query_rule ] "ezmysql_ppx"
