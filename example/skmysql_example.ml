open Rresult

module Result = struct
  include Result

  let get_ok = function
    | Ok x -> x
    | Error exn -> Fmt.failwith "ERROR: %a:" R.pp_msg exn
end

let () =
  Logs.Src.set_level Skmysql.log_src (Some Debug);
  Printexc.record_backtrace true

let uri = Uri.of_string (Sys.getenv "SKMYSQL_URL")

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

let example conn =
  let trace = Skapm.Trace.init () in
  let skint = Random.bits () in
  let (_, transaction) =
    Skapm.Transaction.make_transaction ~trace ~name:"main" ~type_:"function" ()
  in
  let apm = `Transaction transaction in
  Table.insert ~apm conn { skint; skstr = "skmysql" } |> Result.get_ok;

  let (_ : Table_def.t list) =
    Table.select ~apm conn "where skint = %a" Skmysql.Pp.int skint
    |> Result.get_ok
  in
  let (_ : [ `Msg of string ]) =
    Skmysql.get ~apm conn
      (Skmysql.select [ "skstr" ] ~from:"skmysql" "where sks = 'skmysql'")
    |> Result.get_error
  in

  Table.insert_many ~apm ~on_duplicate_key_update:`All conn
    [ { skint = 1; skstr = "str1" }; { skint = 2; skstr = "str2" } ]
  |> Result.get_ok;

  let () =
    let query =
      Skmysql.make_get
        "SELECT skmysql.skint AS s_id, skmysql.skstr FROM skmysql"
    in
    let rows = Skmysql.get conn query in
    let rows = Result.get_ok rows in
    let row = List.hd rows in
    let (_ : int) = row |> Skmysql.get_column ~alias:"s_id" Table_def.skint in
    ()
  in

  let (_ : Skapm.Transaction.result) =
    Skapm.Transaction.finalize_and_send transaction
  in
  ()

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.Src.set_level Skmysql.log_src (Some Debug);
  Logs.set_reporter @@ Logs_fmt.reporter ();
  let conn = Skmysql.connect uri |> Result.get_ok in
  Skmysql.Table.create_exn conn Table.table ~ok_if_exists:true;
  let apm_service_name = "skmysql_example" in
  let apm_secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let apm_url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Skapm.Context.make ~secret_token:apm_secret_token
      ~service_name:apm_service_name ~apm_server:apm_url ()
  in
  Skapm.Apm.init context;
  example conn
