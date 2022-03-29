open Ppxlib

let table_name =
  Attribute.declare "table_name" Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
    (fun str _ _ -> str)

let column_name =
  Attribute.declare "column_name" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
    (fun str _ _ -> str)

let primary_key =
  Attribute.declare "primary_key" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let auto_increment =
  Attribute.declare "auto_increment" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let nullable =
  Attribute.declare "nullable" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let repr =
  Attribute.declare "repr" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
