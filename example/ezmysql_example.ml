open Rresult
open Lwt

module Result = struct
  include Result

  let get_ok = function
    | Ok x -> x
    | Error exn -> Fmt.failwith "ERROR: %a:" R.pp_msg exn
end

let () = Printexc.record_backtrace true

let uri = Uri.of_string (Sys.getenv "EZMYSQL_URL")
let conn = Ezmysql.connect uri |> Result.get_ok

module Table_def = struct
  let ezint = Ezmysql.Column.make_int "ezint" Ezmysql.Column.Conv.identity
  let ezstr =
    Ezmysql.Column.make_varchar "ezstr" 64 Ezmysql.Column.Conv.identity

  let table = Ezmysql.Table.make "ezmysql" [ Pack ezint; Pack ezstr ]

  type t = {
    ezint : int;
    ezstr : string;
  }

  let to_row (t : t) =
    Ezmysql.row_of_list
      [ Ezmysql.pack_column ezint t.ezint; Ezmysql.pack_column ezstr t.ezstr ]

  let of_row row =
    let get c = Ezmysql.get_column c row in
    try Ok { ezint = get ezint; ezstr = get ezstr } with
    | _ -> R.error_msgf "Invalid %a row" Ezmysql.Pp.table_name table
end

module Table = Ezmysql.Make (Table_def)

let example =
  ( Lwt.return_unit >|= fun () ->
    let trace = Elastic_apm.Trace.init () in
    let ezint = Random.bits () in
    let (_, transaction) =
      Elastic_apm.Transaction.make_transaction ~trace ~name:"main"
        ~type_:"function" ()
    in
    Table.insert ~transaction ~on_duplicate_key_update:`All conn
      { ezint; ezstr = "ezmysql" }
    |> Result.get_ok;

    let (_ : Table_def.t list) =
      Table.select ~transaction conn "where ezint = %a" Ezmysql.Pp.int ezint
      |> Result.get_ok
    in

    Table.insert_many ~transaction ~on_duplicate_key_update:`All conn
      [ { ezint = 1; ezstr = "str1" }; { ezint = 2; ezstr = "str2" } ]
    |> Result.get_ok;

    let (_ : Elastic_apm.Transaction.result) =
      Elastic_apm.Transaction.finalize_and_send transaction
    in
    ()
  )
  >>= fun () -> Lwt_unix.sleep 10.

let () =
  let apm_service_name = "ezmysql_example" in
  let apm_secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let apm_url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Elastic_apm.Context.make ~secret_token:apm_secret_token
      ~service_name:apm_service_name ~apm_server:apm_url ()
  in
  Elastic_apm.Apm.init context;
  Lwt_main.run example |> ignore
