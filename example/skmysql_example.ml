open Rresult
open Lwt

module Result = struct
  include Result

  let get_ok = function
    | Ok x -> x
    | Error exn -> Fmt.failwith "ERROR: %a:" R.pp_msg exn
end

let () = Printexc.record_backtrace true

let uri = Uri.of_string (Sys.getenv "SKMYSQL_URL")
let conn = Skmysql.connect uri |> Result.get_ok

module Table_def = struct
  let skint = Skmysql.Column.make_int "skint" Skmysql.Column.Conv.identity
  let skstr =
    Skmysql.Column.make_varchar "skstr" 64 Skmysql.Column.Conv.identity

  let table = Skmysql.Table.make "skmysql" [ Pack skint; Pack skstr ]

  type t = {
    skint : int;
    skstr : string;
  }

  let to_row (t : t) =
    Skmysql.row_of_list
      [ Skmysql.pack_column skint t.skint; Skmysql.pack_column skstr t.skstr ]

  let of_row row =
    let get c = Skmysql.get_column c row in
    try Ok { skint = get skint; skstr = get skstr } with
    | _ -> R.error_msgf "Invalid %a row" Skmysql.Pp.table_name table
end

module Table = Skmysql.Make (Table_def)

let example =
  ( Lwt.return_unit >|= fun () ->
    let trace = Elastic_apm.Trace.init () in
    let skint = Random.bits () in
    let (_, transaction) =
      Elastic_apm.Transaction.make_transaction ~trace ~name:"main"
        ~type_:"function" ()
    in
    Table.insert ~apm:transaction conn { skint; skstr = "skmysql" }
    |> Result.get_ok;

    let (_ : Table_def.t list) =
      Table.select ~apm:transaction conn "where skint = %a" Skmysql.Pp.int skint
      |> Result.get_ok
    in
    let (_ : [ `Msg of string ]) =
      Skmysql.get ~apm:transaction conn
        (Skmysql.select [ "skstr" ] ~from:"skmysql" "where sks = 'skmysql'")
      |> Result.get_error
    in

    Table.insert_many ~apm:transaction ~on_duplicate_key_update:`All conn
      [ { skint = 1; skstr = "str1" }; { skint = 2; skstr = "str2" } ]
    |> Result.get_ok;

    let (_ : Elastic_apm.Transaction.result) =
      Elastic_apm.Transaction.finalize_and_send transaction
    in
    ()
  )
  >>= fun () -> Lwt_unix.sleep 10.

let () =
  let apm_service_name = "skmysql_example" in
  let apm_secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let apm_url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Elastic_apm.Context.make ~secret_token:apm_secret_token
      ~service_name:apm_service_name ~apm_server:apm_url ()
  in
  Elastic_apm.Apm.init context;
  Lwt_main.run example |> ignore
