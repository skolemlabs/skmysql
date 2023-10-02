module Utils = struct
  let get ?alias table_name c row =
    match Skmysql.find_column ?alias c row with
    | Ok col_opt ->
      let col_name = c |> Skmysql.Column.of_spec |> Skmysql.Column.name in
      col_opt
      |> Option.to_result
           ~none:
             (Rresult.R.msgf "table (%s) does not have column: (%s)" table_name
                col_name
             )
    | Error _ as err -> err

  let result_map f items =
    List.fold_left
      (fun acc x ->
        match acc with
        | Error _ as e -> e
        | Ok results ->
          ( match f x with
          | Error _ as e -> e
          | Ok r -> Ok (r :: results)
          )
      )
      (Ok []) items
end

module Table = struct
  let identity = Skmysql.Column.Conv.identity
  let id = Skmysql.Column.make_int "id" identity
  let field = Skmysql.Column.make_blob "field" identity
  let field_float = Skmysql.Column.make_float "field_float" identity

  type t = {
    id : int;
    field : string;
    field_float : float;
  }

  let to_string e =
    "id: "
    ^ (e.id |> string_of_int)
    ^ " field: "
    ^ e.field
    ^ " field_float: "
    ^ (e.field_float |> string_of_float)

  let table =
    Skmysql.Table.make "sample_table" [ Pack id; Pack field; Pack field_float ]

  let to_row (e : t) =
    Skmysql.row_of_list
      [
        Skmysql.pack_column id e.id;
        Skmysql.pack_column field e.field;
        Skmysql.pack_column field_float e.field_float;
      ]

  let of_row row =
    let ( let* ) = Result.bind in
    let get c = Utils.get "sample_table" c row in
    let* id = get id in
    let* field = get field in
    let* field_float = get field_float in
    { id; field; field_float } |> Result.ok

  include Skmysql.Make (struct
    type nonrec t = t
    let table = table
    let to_row = to_row
    let of_row = of_row
  end)
end

let test db () =
  let ( let* ) = Result.bind in
  print_endline "before prepare";
  let* query =
    Skmysql.Prepared.make_get db "SELECT * FROM sample_table WHERE id = ?"
  in
  print_endline "after prepare";
  let fields = [ Skmysql.pack_field Table.id 1 ] in
  let* rows = Skmysql.Prepared.get query fields in
  let* rows = rows |> Utils.result_map Table.of_row in
  rows |> Result.ok

let () =
  let uri = Uri.of_string (Sys.getenv "DB") in
  let conn = Skmysql.connect_exn uri in
  let res = test conn () in
  match res with
  | Error (`Msg s) -> print_endline s
  | Ok rows ->
    rows |> List.iter (fun row -> row |> Table.to_string |> print_endline)
