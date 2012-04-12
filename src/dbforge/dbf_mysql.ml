(* $Id: dbf_mysql.ml 195 2006-05-30 12:15:59Z zoggy $ *)

let debug_printer = ref (fun q -> ())

open Dbf_sql_driver
module MysqlDriver : SqlDriver =
struct
  type db     = Mysql.dbd
  type cursor = Mysql.result

  type result =
    | R_Ok
    | R_Empty
    | R_Fetch of cursor

  type fetch_mode =
    | FM_Array
    | FM_Assoc
    | FM_ListAssoc

  type fetch_result =
    | FR_Array     of string option array
    | FR_Assoc     of string option StringMap.t
    | FR_ListAssoc of (string * string option) array

  type transaction =
    | T_ReadCommited
    | T_Serializable

  type sql_value = string

  let db_id = "MySQL"

  let identifier_re = Str.regexp "^[[:alnum:]_]+$"

  let escape_identifier = fun name ->
    if not (Str.string_match identifier_re name 0) then
      invalid_arg "Invalid MySQL identifier";
    Printf.sprintf "`%s`" name

  let escape_value = fun value ->
    let buffer = Buffer.create 255 in
      for i = 0 to (String.length value) - 1 do
        match value.[i] with
          | '"'  -> Buffer.add_string buffer "\\\""
          | '\\' -> Buffer.add_string buffer "\\\\"
          | c    -> Buffer.add_char   buffer c
      done;
      Printf.sprintf "\"%s\"" (Buffer.contents buffer)

  let check_for_exceptions = fun doit ->
    try  doit ()
    with Mysql.Error s -> raise (Sql_error s)

  let connect = fun ?host ?database ?port ?password ?user () ->
    let doit = fun () ->
      Mysql.quick_connect ?host ?database ?port ?password ?user ()
    in
      check_for_exceptions doit

  let disconnect = fun db ->
    check_for_exceptions (fun () -> Mysql.disconnect db)

  let exec = fun db ~query ->
    !debug_printer (Printf.sprintf "QUERY: %s" query);
    let result = check_for_exceptions (fun () -> Mysql.exec db query)
    in
      match Mysql.status db with
        | Mysql.StatusOK      -> R_Fetch result
        | Mysql.StatusEmpty   -> R_Empty
        | Mysql.StatusError s ->
            raise (Sql_error (Misc.unopt (Mysql.errmsg db)))

  let format_row = fun fm result array ->
    match fm with
      | FM_Array -> FR_Array array
      | FM_Assoc ->
          FR_Assoc
            (Misc.Array.fold_left2
               (fun map name value -> StringMap.add name value map)
               StringMap.empty
               (Mysql.names result) array)
      | FM_ListAssoc ->
          FR_ListAssoc
            (Misc.Array.combine (Mysql.names result) array)

  let fetch_row = fun ?(fm = FM_Array) result ->
    let doit = fun () ->
      match Mysql.fetch result with
        | None   -> None
        | Some a -> Some (format_row fm result a)
    in
      check_for_exceptions doit

  let fetch_all = fun ?(fm = FM_Array) result ->
    Mysql.map result (format_row fm result)

  let map = fun ?(fm = FM_Array) ~f result ->
    Mysql.map result (fun row -> f (format_row fm result row))

  let begin_transaction = fun db transaction ->
    let s = match transaction with
      | T_ReadCommited -> "READ COMMITED"
      | T_Serializable -> "SERIALIZABLE"
    in
    let query = Printf.sprintf "START TRANSACTION ISOLATION LEVEL %s" s in
      ignore (exec db query)

  let end_transaction    = fun db -> ignore (exec db "COMMIT")
  let cancel_transaction = fun db -> ignore (exec db "ROLLBACK")

  let names  = fun result -> Mysql.names  result
  let fields = fun result -> Mysql.fields result

  let set_autocommit = fun db ~autocommit ->
    let b = if autocommit then 1 else 0 in
    let query = Printf.sprintf "SET AUTOCOMMIT=%d" b in
      ignore (exec db query)

  (*================\
  | Data conversion |
  \================*)
  let bool_of_int = fun i -> i <> 0
  let int_of_bool = function | true -> 1 | false -> 0

  let str2sql   = fun v -> (v : sql_value)
  let bool2sql  = fun v -> string_of_int (int_of_bool v)
  let int2sql   = fun v -> string_of_int v
  let int322sql = fun v -> (Int32.to_string v)
  let int642sql = fun v -> (Int64.to_string v)
  let float2sql = fun v -> string_of_float v

  let check_conversion = fun doit ->
    try  doit ()
    with Failure _ -> raise Invalid_conversion

  let sql2str   = fun s -> (s : string)
  let sql2bool  = fun s -> check_conversion (fun () -> bool_of_int (int_of_string s))
  let sql2int   = fun s -> check_conversion (fun () -> int_of_string s)
  let sql2int32 = fun s -> check_conversion (fun () -> Int32.of_string s)
  let sql2int64 = fun s -> check_conversion (fun () -> Int64.of_string s)
  let sql2float = fun s -> check_conversion (fun () -> float_of_string s)
end
