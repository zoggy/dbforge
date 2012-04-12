<# [
open Dbf_sql.SQL_db
open Dbf_sql
] #>

<?block [
let (table,module_name,_idx) = tmpl__env in

let pkey    =
  match table.SQL_db.ta_pkey with
    | [\] -> None
    | pkey -> Some { SQL_db.idx_name    = "";
                     SQL_db.idx_columns = pkey;
                     SQL_db.idx_unique  = true;
                     SQL_db.idx_db      = table.SQL_db.ta_db; }

and sql_pkey =
  if table.SQL_db.ta_pkey <> [\] then
    Some (Dbf_misc.join
            ~sep:", "
            ~to_string:(fun c -> c.col_name)
            table.SQL_db.ta_pkey)
  else
    None
in

let indexes =
  (match pkey with None -> [\] | Some i -> [i\]) @ _idx
and columns = table.ta_columns
in

let table_name  = table.ta_name in
let log_table_name = Printf.sprintf "%s_log" table_name in
let log_table =
  { table with
    ta_name = log_table_name ;
    ta_logged = false ;
    ta_pkey = [\] ;
  }
in
let log_columns =
  let col ~name ~ty ~ocaml_ty ~sql2ml ~ml2sql =
    { col_name = name ;
      col_comment = "" ;
      col_type = ty ;
      col_table = log_table ;
      col_nullable = false ;
      col_spec_options = Dbf_misc.StringMap.empty;
      col_spec_ty = Dbf_misc.StringMap.empty;
      col_ocaml_ty = ocaml_ty ;
      col_sql2ml = sql2ml ;
      col_ml2sql = ml2sql ;
    }
  in
  [
    col ~name: "log_who" ~ty: (Int (None, NO_None))
      ~ocaml_ty: "int"
      ~sql2ml: "Sql.sql2int" ~ml2sql: "Sql.int2sql" ;

    col ~name: "log_date" ~ty: (SQL_db.Double (None, NO_None))
      ~ocaml_ty: "float"
      ~sql2ml: "Sql.sql2float" ~ml2sql: "Sql.float2sql" ;

    col ~name: "log_action" ~ty: (SQL_db.TinyInt (None,NO_None))
      ~ocaml_ty: "Dbf_sql_misc.log_action"
      ~sql2ml: "Dbf_sql_misc.action_of_string"
      ~ml2sql: "Dbf_sql_misc.string_of_action";
  \]
in
log_table.ta_columns <- log_columns @ table.ta_columns;

let idx_name    = fun index -> index.idx_name

and col_name     = fun column -> column.col_name
and col_ocaml_ty = fun opt column ->
  if opt && column.col_nullable then
    Printf.sprintf "%s option" column.col_ocaml_ty
  else
    column.col_ocaml_ty
in

let args_of_columns = fun ~nullty ~opt columns ->
  let print =
    if   opt
    then (fun s1 s2 -> Printf.sprintf "?(%s : %s option)" s1 s2)
    else (fun s1 s2 -> Printf.sprintf "~(%s : %s)" s1 s2)
  in
    Dbf_misc.join
      ~sep:" "
      ~to_string:
        (fun c -> print c.col_name (col_ocaml_ty nullty c))
      columns

and sql_columns_spec_infos =
  let sql_column_spec_infos = fun column ->
    let db2 =
      Dbf_misc.StringMap.fold (fun k _ acc -> k :: acc)
        column.SQL_db.col_spec_ty [\]
    and db1 =
      Dbf_misc.StringMap.fold (fun k _ acc -> k :: acc)
        column.SQL_db.col_spec_options [\]
    in
    let dbs = Dbf_misc.uniq ~sorted:false (db1 @ db2) in
    let infos_for_db = fun db ->
      let spec_ty =
        try  Printf.sprintf "Some \"%s\""
          (String.escaped
             (Dbf_misc.StringMap.find db column.SQL_db.col_spec_ty))
        with Not_found ->
          "None"
      and spec_options =
        try
          Printf.sprintf "Some \"%s\""
            (String.escaped
               (Dbf_misc.join ~sep:" " ~to_string:(fun x -> x)
                  (Dbf_misc.StringMap.find db column.SQL_db.col_spec_options)))
        with
          | Not_found ->
              "None"
      in
        (Printf.sprintf "\"%s\"" (String.escaped db),
         Printf.sprintf "(%s, %s)" spec_ty spec_options)
    in
      List.map infos_for_db dbs
  in
    List.map
      (fun c -> (Printf.sprintf "\"%s\""
                   (String.escaped c.SQL_db.col_name),
                 Printf.sprintf "\"%s %s\""
                   (String.escaped (SQL_ty.fullstring_of_type c.col_type))
                   (if c.SQL_db.col_nullable then "NULL" else "NOT NULL"),
                 sql_column_spec_infos c))
      columns

and sql_columns =
  Dbf_misc.join
    ~sep:", "
    ~to_string:(fun c -> c.col_name)
    columns

and idx_sql_columns = fun index ->
  Dbf_misc.join ~sep:", " ~to_string:(fun c -> c.col_name) index.idx_columns
]?>

module <![module_name]!> = functor (Sql : Dbf_sql_driver.SqlDriver) ->
  struct
    type t = {
        <?iter:name=column [columns]?>
        mutable <![col_name column]!> : <![col_ocaml_ty true column]!> ;
        <?/iter?>
      }

    let opt_values_of_args = fun <![args_of_columns true true columns]!> () ->
      [<?iter:name=c [columns]?>
        begin match <![col_name c]!> with
        | None   -> None
        | Some v -> begin
            <?if [c.col_nullable] ?>
              match v with
              | None   ->
                  Some (None,
                        "<![col_name c]!>")
              | Some v ->
                  Some (Some (Sql.escape_value ((<![c.col_ml2sql]!>) v)),
                        "<![col_name c]!>")
                    <?/if?><?if [not c.col_nullable]?>
                      Some (Some (Sql.escape_value ((<![c.col_ml2sql]!>) v)),
                            "<![col_name c]!>")
                        <?/if?>
        end
        end ;
        <?/iter?>]

    let condition_of_args = fun <![args_of_columns true true columns]!> () ->
      let vals = opt_values_of_args
          <?iter:name=c [columns]?>?<![c.col_name]!> <?/iter?> ()
      in
      let columns = List.fold_left
          (fun acc opt ->
            match opt with
              None -> acc
            | Some (v,name) -> (v,String.escaped name) :: acc)
          []
          vals
      in
      Dbf_sql_misc.join
        ~sep:" AND "
        ~to_string:
        (function
            (None, name) -> Printf.sprintf "%s IS NULL" name
          | (Some value, name)->
              Printf.sprintf "%s = %s" name value
        )
        columns

    let columns_decls =
      [<?iter:name=c [sql_columns_spec_infos]?>
        <?block [let (name, ty, infos) = c]?>
        (<![name]!>, <![ty]!>,
         [<?iter:name=info [infos]?>(<![fst info]!>, <![snd info]!>);<?/iter?>]) ;
        <?/block?><?/iter?>]


    let log_sql_columns_decls =
      let sql_column_decl = fun (name, ty, opts) ->
        let ty =
          try
            match List.assoc Sql.db_id opts with
            | None,         _ -> ty
            | Some spec_ty, _ -> spec_ty
          with
          | Not_found -> ty
        in
        Printf.sprintf "%s %s" name ty
      in
      Dbf_sql_misc.join
        ~sep:", "
        ~to_string:sql_column_decl
        columns_decls

    let sql_columns_decls =
      let sql_column_decl = fun (name, ty, opts) ->
        let (ty, options) =
          try
            match List.assoc Sql.db_id opts with
            | None,         None      -> (ty, "")
            | Some spec_ty, None      -> (spec_ty, "")
            | None,         Some opts -> (ty, opts)
            | Some spec_ty, Some opts -> (spec_ty, opts)
          with
          | Not_found -> (ty, "")
        in
        Printf.sprintf "%s %s %s" name ty options
      in
      let decls =
        Dbf_sql_misc.join
          ~sep:", "
          ~to_string:sql_column_decl
          columns_decls
      in
      <?if [sql_pkey <> None]?>
        let decls = Printf.sprintf "%s, PRIMARY KEY (%s)"
            decls "<![String.escaped (Dbf_misc.unopt sql_pkey)]!>" in
        <?/if?>
          decls

    let row_as_record = fun ?(offset = 0) row ->
      {
        <?for:name=idx [(0, List.length columns - 1)]?>
        <?block [let column = List.nth columns idx]?>
        <![col_name column]!> =
        <?if [column.col_nullable]?>
          Dbf_sql_misc.apply_opt
            (<![column.col_sql2ml]!>) row.(<![string_of_int (idx)]!> + offset) ;
        <?/if?><?if [not column.col_nullable]?>
          (<![column.col_sql2ml]!>)
            (Dbf_sql_misc.unopt row.(<![string_of_int (idx)]!> + offset)) ;
        <?/if?>
          <?/block?>
          <?/for?>
          }

    <?if [table.ta_logged]?>
    let drop_log = fun db ->
      ignore (Sql.exec db "DROP TABLE <![table_name]!>")
    <?/if?>

    let drop = fun db ->
      ignore (Sql.exec db "DROP TABLE <![log_table_name]!>")
      <?if [table.ta_logged]?>; drop_log db<?/if?>

    <?if [table.ta_logged]?>
    let create_log = fun db ->
      let decls = Printf.sprintf "%s,%s,%s,%s"
          "log_who int not null"
          "log_date double not null"
          "log_action tinyint not null"
          log_sql_columns_decls
      in
      let q = Printf.sprintf "CREATE TABLE <![log_table_name]!> ( %s )" decls in
      ignore (Sql.exec db q)
    <?/if?>

    let create = fun db ->
      let create_table = fun () ->
        ignore (
        Sql.exec db
          (Printf.sprintf
             "CREATE TABLE <![table_name]!> ( %s )"
             sql_columns_decls)
       )
          <?iter:name=idx [indexes]?><?if [idx.SQL_db.idx_name <> ""]?>
      and create_index_<![idx_name idx]!> = fun () ->
        ignore (
        Sql.exec db
          ("CREATE <?if [idx.idx_unique]?>UNIQUE<?/if?> INDEX " ^
           "<![idx_name idx]!> ON <![table_name]!> " ^
           "( <![idx_sql_columns idx]!> )")
       )
          <?/if?><?/iter?>
      in begin
        create_table ();
        <?if [table.ta_logged]?>create_log db;<?/if?>
        <?iter:name=index [indexes]?><?if [index.SQL_db.idx_name <> ""]?>
          create_index_<![idx_name index]!> ();
        <?/if?><?/iter?>
      end

    let select_where = fun db ?(table_alias="") condition ->
      let query =
        Printf.sprintf
          "SELECT <![sql_columns]!> FROM <![table_name]!> %s %s"
          table_alias
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

    let select = fun db <?iter:name=col [columns]?>?<![col.col_name]!> <?/iter?>() ->
      let cond = condition_of_args
          <?iter:name=col [columns]?>?<![col.col_name]!> <?/iter?>()
      in
      match select_where db cond with
      | Sql.R_Ok
      | Sql.R_Empty -> []
      | Sql.R_Fetch cursor -> fetch_all cursor

    <?if [table.ta_logged]?>
    let log_row_as_record = fun ?(offset = 0) row ->
      let t = row_as_record ~offset: (offset+3) row in
      (Sql.sql2int (Dbf_sql_misc.unopt row.(0)),
       Sql.sql2float (Dbf_sql_misc.unopt row.(1)),
       Dbf_sql_misc.action_of_string (Dbf_sql_misc.unopt row.(2)),
       t
      )

    let log_fetch_all = fun result ->
      let to_array = function
        | Sql.FR_Array a -> a
        | _ -> Dbf_sql_misc.ie ()
      in
      Sql.map result ~f:(fun r -> log_row_as_record (to_array r))

    let select_log_where = fun db ?(table_alias="") condition ->
      let query =
        Printf.sprintf
          "SELECT log_who, log_date, log_action, <![sql_columns]!> FROM <![log_table_name]!> %s %s"
          table_alias
          (match Dbf_sql_misc.no_blanks condition with
            "" -> ""
          | _ -> Printf.sprintf "WHERE %s" condition
          )
      in
      Sql.exec db query

    let select_log = fun db <?iter:name=col [columns]?>?<![col.col_name]!> <?/iter?>() ->
      let cond = condition_of_args
          <?iter:name=col [columns]?>?<![col.col_name]!> <?/iter?>()
      in
      match select_log_where db cond with
      | Sql.R_Ok
      | Sql.R_Empty -> []
      | Sql.R_Fetch cursor -> log_fetch_all cursor

    let insert_log = fun db log_action log_date ->
      let log_who = !log_who () in
      fun t ->
        let columns =
          opt_values_of_args
            <?iter:name=c [columns]?><!["~" ^ (col_name c) ^ ": t." ^ (col_name c)]!><?/iter?>
          ()
        in
        let columns = List.fold_left
            (fun acc opt ->
              match opt with
                None -> acc
              | Some (None,name) -> ("NULL",name)::acc
              | Some (Some v,name) -> (v,name)::acc
            )
            []
            columns
        in
        let columns =
          (Sql.escape_value (Sql.int2sql log_who), "log_who") ::
          (Sql.escape_value (Sql.float2sql log_date), "log_date") ::
          (Sql.escape_value (Dbf_sql_misc.string_of_action log_action), "log_action") ::
          columns
        in
        let query =
          Printf.sprintf "INSERT INTO <![log_table_name]!> (%s) VALUES (%s)"
            (Dbf_sql_misc.join ~sep:", " ~to_string:snd columns)
            (Dbf_sql_misc.join ~sep:", " ~to_string:fst columns)
        in
        ignore (Sql.exec db query)
    <?/if?>

    let delete_where = fun db ?(table_alias="") condition ->
      <?if [table.ta_logged]?>
      let impacted = match select_where db ~table_alias condition with
        | Sql.R_Ok
        | Sql.R_Empty -> []
        | Sql.R_Fetch cursor -> fetch_all cursor
      in
      <?/if?>
      let query =
        Printf.sprintf
          "DELETE FROM <![table_name]!> %s %s"
          table_alias
          (match Dbf_sql_misc.no_blanks condition with
            "" -> ""
          | _ -> Printf.sprintf "WHERE %s" condition
          )
      in
      ignore (Sql.exec db query);
      <?if [table.ta_logged]?>
      List.iter (insert_log db Dbf_sql_misc.Delete (Unix.time())) impacted;
      <?/if?>
      ()

    let delete = fun db <![args_of_columns true true columns]!> () ->
      let cond = condition_of_args
          <?iter:name=col [columns]?>?<![col.col_name]!> <?/iter?>()
      in
      delete_where db cond

    let update = fun db <?iter:name=col [columns]?>
      ?key_<![col.col_name]!>
      <?/iter?><![args_of_columns true true columns]!> () ->
        let cond = condition_of_args
            <?iter:name=col [columns]?>?<![col.col_name]!>: key_<![col.col_name]!> <?/iter?>()
        and out_columns =
          opt_values_of_args
            <?iter:name=c [columns]?><!["?" ^ (col_name c)  ^ " "]!><?/iter?>
          ()
        in
        <?if [table.ta_logged]?>
        let impacted = match select_where db cond with
        | Sql.R_Ok
        | Sql.R_Empty -> []
        | Sql.R_Fetch cursor -> fetch_all cursor
        in
        <?/if?>
        let query =
          Printf.sprintf "UPDATE <![table_name]!> SET %s %s"
            (Dbf_sql_misc.join_opt
               ~sep:" , "
               ~to_string:
               (function
                 | (None, name) -> Printf.sprintf "%s = NULL" name
                 | (Some value, name) ->
                     Printf.sprintf "%s = %s" name value
               )
               out_columns
            )
            (match Dbf_sql_misc.no_blanks cond with
              "" -> ""
            | _ -> Printf.sprintf "WHERE %s" cond
            )
        in
        ignore (Sql.exec db query);
        <?if [table.ta_logged]?>
        List.iter (insert_log db Dbf_sql_misc.Update (Unix.time())) impacted;
        <?/if?>
        ()

    let insert = fun db <![args_of_columns true true columns]!> () ->
      let selected_columns =
        opt_values_of_args
          <?iter:name=c [columns]?><!["?" ^ (col_name c) ^ " "]!><?/iter?>
        ()
      in
      let selected_columns = List.map
          (function None -> None
            | Some (None,name) -> Some ("NULL",name)
            | Some (Some v,name) -> Some (v,name)
          )
          selected_columns
      in
      let query =
        Printf.sprintf "INSERT INTO <![table_name]!> (%s) VALUES (%s)"
          (Dbf_sql_misc.join_opt ~sep:", " ~to_string:snd selected_columns)
          (Dbf_sql_misc.join_opt ~sep:", " ~to_string:fst selected_columns)
      in
      ignore (Sql.exec db query);
      <?if [table.ta_logged]?>
      let selected_columns =
        (Some (Sql.escape_value (Sql.int2sql (!log_who())), "log_who")) ::
        (Some (Sql.escape_value (Sql.float2sql (Unix.time())), "log_date")) ::
        (Some (Sql.escape_value (Dbf_sql_misc.string_of_action Dbf_sql_misc.Insert), "log_action")) ::
        selected_columns
      in
      let query =
        Printf.sprintf "INSERT INTO <![log_table_name]!> (%s) VALUES (%s)"
          (Dbf_sql_misc.join_opt ~sep:", " ~to_string:snd selected_columns)
          (Dbf_sql_misc.join_opt ~sep:", " ~to_string:fst selected_columns)
      in
      ignore (Sql.exec db query);
      <?/if?>
      ()

  (*=======================================\
     | Insertion/update/deletion with indexes |
     \=======================================*)
        <?iter:name=idx [indexes]?><?if [idx.idx_unique]?>
          <?block [
          let (in_idx, out_idx) =
            List.partition (fun c -> List.memq c idx.idx_columns) columns
          and module_name =
            if idx.SQL_db.idx_name = "" then
              "PKey"
            else
              Printf.sprintf "I_%s" (idx_name idx)
        ]?>
    module <![module_name]!> =
      struct
        let delete = fun db <![args_of_columns false false in_idx]!> ->
          let condition =
            condition_of_args <?iter:name=c [in_idx]?>~<![col_name c]!> <?/iter?> ()
          in
          delete_where db condition

        let search = fun db <![args_of_columns false false in_idx]!> ->
          let condition =
            (condition_of_args <?iter:name=c [in_idx]?>~<![col_name c]!> <?/iter?> ())
          in
          match select_where db condition with
          | Sql.R_Fetch r -> fetch r
          | _             -> None

                <?if [out_idx <> [\]] ?>
                  let update = fun db <![args_of_columns false false in_idx]!>
                    <![args_of_columns true true out_idx]!>
                    () ->
                      let in_columns =
                        [<?iter:name=c [in_idx]?>
                          (Sql.escape_value ((<![c.col_ml2sql]!>) <![col_name c]!>),
                           "<![String.escaped (col_name c)]!>") ;
                          <?/iter?>]
                      and out_columns =
                        opt_values_of_args
                          <?iter:name=c [out_idx]?><!["?" ^ (col_name c)  ^ " "]!><?/iter?>
                        ()
                      in
                      let query =
                        Printf.sprintf "UPDATE <![table_name]!> SET %s WHERE %s"
                          (Dbf_sql_misc.join_opt
                             ~sep:" , "
                             ~to_string:
                             (function
                               | (None, name) -> Printf.sprintf "%s = NULL" name
                               | (Some value, name) ->
                                   Printf.sprintf "%s = %s" name value
                             )
                             out_columns)
                          (Dbf_sql_misc.join
                             ~sep:" AND "
                             ~to_string:
                             (fun (value, name)-> Printf.sprintf "%s = %s" name value)
                             in_columns)
                      in
                      ignore (Sql.exec db query)
                        <?/if?>

      end
        <?/block?><?/if?><?/iter?>

  end
    <?/block?>
