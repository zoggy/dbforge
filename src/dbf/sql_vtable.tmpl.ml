<# [
open Dbf_sql.SQL_db
open Dbf_sql
] #>

<?block [
let vtable  = fst tmpl__env
and indexes = snd tmpl__env
in

let tables =
  vtable.vt_ftable :: (List.map fst vtable.vt_join)

and vtable_name = vtable.vt_name
and table_name  = fun t -> t.ta_name
and index_name  = fun i -> i.idx_name
and column_name = fun c -> c.col_name
and vtable_sql  = string_of_vtable vtable
in

let sql_columns =
  Dbf_misc.join ~sep:", " ~to_string:(fun x -> x)
    (List.flatten
       (List.map (fun t -> List.map column_fullname t.ta_columns) tables))
in

let args_of_columns = fun columns ->
  let print =
    fun s1 s2 -> Printf.sprintf "~(%s : %s)" s1 s2
  in
    Dbf_misc.join
      ~sep:" "
      ~to_string:
        (fun c -> print c.col_name c.col_ocaml_ty)
      columns

]?>

module VT_<![vtable_name]!> = functor (Sql : Dbf_sql_driver.SqlDriver) ->
struct
  <?iter:name=table [tables]?>
  module T_<![table_name table]!> = T_<![table_name table]!> (Sql)
  <?/iter?>

  type t = {
    <?iter:name=table [tables]?>
    <![table_name table]!> : T_<![table_name table]!>.<![table_name table]!> ;
    <?/iter?>
  }

  let row_as_record = fun row ->
    let offset = ref 0 in
    <?iter:name=table [tables]?>
      let <![table_name table]!> =
        T_<![table_name table]!>.row_as_record ~offset:!offset row
      in
        offset := !offset + <![string_of_int (List.length table.ta_columns)]!> ;
    <?/iter?>
        {<?iter:name=table [tables]?>
          <![table_name table]!> = <![table_name table]!> ;
        <?/iter?>}

  let select_where = fun db condition ->
    let query =
      Printf.sprintf
        "SELECT <![sql_columns]!> FROM <![vtable_sql]!> %s"
        (match Dbf_sql_misc.no_blanks condition with
          "" -> ""
        | _ -> Printf.sprintf "WHERE %s" condition
        )
    in
      Sql.exec db query

  let fetch = fun result ->
    match Sql.fetch_row result with
      | None                    -> None
      | Some (Sql.FR_Array row) -> Some (row_as_record row)
      | _                       -> Dbf_sql_misc.ie ()

  let fetch_all = fun result ->
    let to_array = function
      | Sql.FR_Array a -> a
      | _ -> Dbf_sql_misc.ie ()
    in
      Sql.map result ~f:(fun r -> row_as_record (to_array r))

  <?iter:name=idx [indexes]?>
  <?block [
    let (in_idx, out_idx) =
      List.partition
        (fun c -> List.memq c idx.idx_columns)
        (table_of_index idx).ta_columns
  ]?>
  module I_<![index_name idx]!> =
  struct
    let condition_of_args = fun <![args_of_columns in_idx]!> ->
      let columns =
        [<?iter:name=c [in_idx]?>
           ((<![c.col_ml2sql]!>) <![column_name c]!>,
            "<![String.escaped (column_fullname c)]!>") ;
        <?/iter?>]
      in
        Dbf_sql_misc.join
          ~sep:" AND "
          ~to_string:
            (fun (value, name) ->
               Printf.sprintf "%s = %s" name (Sql.escape_value value))
          columns

    let search = fun db <![args_of_columns in_idx]!> ->
      let condition =
        (condition_of_args <?iter:name=c [in_idx]?><![column_name c]!><?/iter?>)
      in
        match select_where db condition with
          | Sql.R_Fetch r -> fetch r
          | _             -> None
  end
  <?/block?>
  <?/iter?>

end
<?/block?>
