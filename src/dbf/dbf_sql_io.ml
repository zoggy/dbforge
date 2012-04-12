(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

open Dbf_sql

exception Invalid_db_file

let checked_dtd = Dtd.check (Dtd.parse_file Dbf_installation.db_dtd_file)

(*==========\
| DB -> XML |
\==========*)
let pcdata_tag = fun ~name ~pcdata ->
  Xml.Element (name, [], [Xml.PCData pcdata])

let xml_of_type = fun ty spec_ty ->
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children in
    new_info (pcdata_tag "name" (SQL_ty.string_of_type ty));
    (match SQL_ty.get_display_size ty with
       | Some i -> new_info (pcdata_tag "dispsize" (string_of_int i))
       | None   -> ());
    (match SQL_ty.get_precision ty with
       | Some i -> new_info (pcdata_tag "precision" (string_of_int i))
       | None   -> ());
    (match SQL_ty.string_of_type_options ty with
       | Some opt -> new_info (pcdata_tag "options" opt)
       | None     -> ());
    Dbf_misc.StringMap.iter
      (fun db ty ->
         new_info (Xml.Element ("spec_ty", ["db", db], [Xml.PCData ty])))
      spec_ty;
    let children = List.rev !children in
      Xml.Element ("type", [], children)

let xml_of_tableref = fun table ->
  pcdata_tag "tableref" table.SQL_db.ta_name

let xml_of_columnref = fun column ->
  pcdata_tag "columnref" column.SQL_db.col_name

let xml_of_columnfullref = fun column ->
  Xml.Element ("columnfullref", [],
               [xml_of_tableref column.SQL_db.col_table;
                xml_of_columnref column])

let xml_of_column = fun column ->
  let spec_options = fun db options ->
    let children = ref [] in
    let new_info = fun tag -> children := tag :: !children in
      List.iter
        (fun option -> new_info (pcdata_tag "spec_option" option))
        options;
      Xml.Element ("spec_options", [("db", db)], List.rev !children)
  in
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children in
    new_info (pcdata_tag "name" column.SQL_db.col_name);
    new_info (pcdata_tag "comment" column.SQL_db.col_comment);
    new_info (xml_of_type column.SQL_db.col_type column.SQL_db.col_spec_ty);
    new_info (pcdata_tag "ocaml_type" column.SQL_db.col_ocaml_ty);
    new_info (pcdata_tag "sql2ml" column.SQL_db.col_sql2ml);
    new_info (pcdata_tag "ml2sql" column.SQL_db.col_ml2sql);
    Dbf_misc.StringMap.iter
      (fun db options -> new_info (spec_options db options))
      column.SQL_db.col_spec_options;
    let children = List.rev !children
    and attrs    = ["nullable", (string_of_bool column.SQL_db.col_nullable)]
    in
      Xml.Element ("column", attrs, children)

let xml_of_table = fun table ->
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children
  and columns  = List.map xml_of_column table.SQL_db.ta_columns in
    new_info (pcdata_tag "name" table.SQL_db.ta_name);
    new_info (pcdata_tag "comment" table.SQL_db.ta_comment);
    new_info (Xml.Element ("columns", [], columns));
    if table.SQL_db.ta_pkey <> [] then begin
      let columnsref =
        let children = ref [] in
        let new_info = fun tag -> children := tag :: !children in
          List.iter
            (fun c -> new_info (xml_of_columnref c)) table.SQL_db.ta_pkey;
          !children
      in
        new_info (Xml.Element ("pkey", [], columnsref))
    end;
    let children = List.rev !children
    and attrs    = ["logged", (string_of_bool table.SQL_db.ta_logged)]
    in
    Xml.Element ("table", attrs, children)

let xml_of_join = fun (table, cs) ->
  let children = ref [] in
  let new_infos = fun tag -> children := tag :: !children in
    new_infos (xml_of_tableref table);
    List.iter
      (fun (c1, c2) ->
         assert (c2.SQL_db.col_table == table); (* Phys. eq *)
         new_infos
           (Xml.Element
              ("columneq", [],
               [xml_of_columnfullref c1; xml_of_columnref c2])))
      cs;
    let children = List.rev !children in
      Xml.Element ("join", [], children)

let xml_of_vtable = fun vtable ->
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children in
    new_info (pcdata_tag "name" vtable.SQL_db.vt_name);
    new_info (xml_of_tableref vtable.SQL_db.vt_ftable);
    List.iter
      (fun join -> new_info (xml_of_join join))
      vtable.SQL_db.vt_join;
    let children = List.rev !children in
      Xml.Element ("vtable", [], children)

let xml_of_index = fun index ->
  let table =
    match index.SQL_db.idx_columns with
      | []      -> Dbf_misc.ie ()
      | hd :: _ -> hd.SQL_db.col_table
  in
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children in
    new_info (pcdata_tag "name" index.SQL_db.idx_name);
    new_info (pcdata_tag "tableref" table.SQL_db.ta_name);
    List.iter
      (fun c -> new_info (pcdata_tag "columnref" c.SQL_db.col_name))
      index.SQL_db.idx_columns;
    let children = List.rev !children in
      Xml.Element ("index",
                   ["unique", string_of_bool index.SQL_db.idx_unique],
                   children)

let xml_of_query = fun query ->
  let children = ref [] in
  let new_info = fun tag -> children := tag :: !children in
    new_info (pcdata_tag "name" query.SQL_db.qry_name);
    new_info (pcdata_tag "querytext" query.SQL_db.qry_query);
    new_info (pcdata_tag "comment" query.SQL_db.qry_comment);
    let children = List.rev !children in
    Xml.Element ("query", [], children)

let xml_of_db = fun db ->
  let tables      = List.map xml_of_table  db.SQL_db.db_tables
  and vtables     = List.map xml_of_vtable db.SQL_db.db_vtables
  and indexes     = List.map xml_of_index  db.SQL_db.db_indexes
  and queries     = List.map xml_of_query  db.SQL_db.db_queries in
  let xml_tables  = Xml.Element ("tables",  [], tables)
  and xml_vtables = Xml.Element ("vtables", [], vtables)
  and xml_indexes = Xml.Element ("indexes", [], indexes)
  and xml_queries = Xml.Element ("queries", [], queries) in
    Xml.Element ("db", [], [xml_tables; xml_vtables; xml_indexes; xml_queries])

(*==========\
| XML -> DB |
\==========*)
let db_int_of_string = fun s ->
  try  int_of_string s
  with Failure "int_of_string" -> raise Invalid_db_file

let db_bool_of_string = fun s ->
  try  bool_of_string s
  with Failure "bool_of_string" -> raise Invalid_db_file

let pcdata_from_tag = fun tag ->
  match tag with
    | Xml.Element (_, _, [Xml.PCData text]) -> text
    | _ -> Dbf_misc.ie ()

let pcdata_from_child_tag_opt =
  let check_for_tag = fun name tag ->
    match tag with
      | Xml.Element (name', _, children)
          when name = name' -> begin
            match children with
              | [Xml.PCData _] -> true
              | _              -> Dbf_misc.ie ()
          end
      | _ -> false
  in
    fun tag name ->
      try
        match tag with
          | Xml.Element (_, _, children) ->
              let c = List.find (check_for_tag name) children in
                Some (pcdata_from_tag c)
          | _ -> Dbf_misc.ie ()
      with
        | Not_found -> None

let pcdata_from_child_tag = fun tag name ->
  match pcdata_from_child_tag_opt tag name with
    | Some t -> t
    | None   -> Dbf_misc.ie ()

let find_first_tag_opt = fun tag name ->
  match tag with
    | Xml.Element (_, _, children) -> begin
        let eq_fct = function
          | (Xml.Element (name', _, _)) when name = name'
              -> true
          | _ -> false
        in
          try
            Some (List.find eq_fct children)
          with
            | Not_found -> None
      end
    | _ ->
        Dbf_misc.ie ()

let find_first_tag = fun tag name ->
  match find_first_tag_opt tag name with
    | None   -> Dbf_misc.ie ()
    | Some v -> v

let find_all_tags = fun tag name ->
  let rec find_all = function
    | ((Xml.Element (name', _, _)) as t) :: tl when name = name' ->
        t :: (find_all tl)
    | _ :: tl -> find_all tl
    | []      -> []
  in
    match tag with
      | Xml.Element (_, _, children) -> find_all children
      | _                            -> Dbf_misc.ie ()

let ty_of_dtd_valid_xml = fun xml ->
  let dispsize  = pcdata_from_child_tag_opt xml "dispsize"
  and precision = pcdata_from_child_tag_opt xml "precision"
  and options   = pcdata_from_child_tag_opt xml "options"
  and name      = pcdata_from_child_tag     xml "name"
  and spec_tys  = find_all_tags             xml "spec_ty"
  in
    try
      let ty =
        SQL_ty.type_of_string
          ?dispsize:(Dbf_misc.apply_opt db_int_of_string dispsize)
          ?precision:(Dbf_misc.apply_opt db_int_of_string precision)
          ?options:options
          name
      and spec_tys =
        List.fold_left
          (fun acc xml ->
             let db = Xml.attrib xml "db"
             and ty = pcdata_from_tag xml in
               Dbf_misc.StringMap.add db ty acc)
          Dbf_misc.StringMap.empty spec_tys
      in
        (ty, spec_tys)
    with
      | SQL_ty.Invalid_type _ -> raise Invalid_db_file

let column_of_dtd_valid_xml = fun table xml ->
  let fetch_spec_options = fun () ->
    let tags = find_all_tags xml "spec_options"
    and result = ref Dbf_misc.StringMap.empty in
    let fetch_spec_options_for_db = fun xml ->
      let db = Xml.attrib xml "db" in
      let fetch_option = function
        | Xml.Element ("spec_option", [], [Xml.PCData option]) ->
            option
        | _ ->
            Dbf_misc.ie ();             (* DTD *)
      in
        result := Dbf_misc.StringMap.add
          db (List.map fetch_option (Xml.children xml))
         !result
    in
      List.iter fetch_spec_options_for_db tags;
      !result
  in
  let name    = pcdata_from_child_tag xml "name"
  and comment = pcdata_from_child_tag xml "comment"
  and (ty, spec_tys) = ty_of_dtd_valid_xml (find_first_tag xml "type")
  and ocamlty = pcdata_from_child_tag xml "ocaml_type"
  and sql2ml  = pcdata_from_child_tag xml "sql2ml"
  and ml2sql  = pcdata_from_child_tag xml "ml2sql"
  and options = fetch_spec_options ()
  in
    try
      let column =
        SQL_db.insert_column table
          ~name:name
          ~comment:comment
          ~ty:ty
          ~nullable:(db_bool_of_string (Xml.attrib xml "nullable"))
          ()
      in
        column.SQL_db.col_spec_options <- options;
        column.SQL_db.col_spec_ty      <- spec_tys;
        column.SQL_db.col_ocaml_ty     <- ocamlty;
        column.SQL_db.col_sql2ml       <- sql2ml;
        column.SQL_db.col_ml2sql       <- ml2sql;
        column
    with
      | SQL_db.Invalid_name _    -> raise Invalid_db_file
      | SQL_db.Duplicated_name _ -> raise Invalid_db_file

let table_of_dtd_valid_xml = fun db xml ->
  let name    = pcdata_from_child_tag xml "name"
  and comment = pcdata_from_child_tag xml "comment"
  and logged  = db_bool_of_string (Xml.attrib xml "logged")
  and columns = find_first_tag xml "columns"
  and pkey    = find_first_tag_opt xml "pkey"
  in
    try
      let table = SQL_db.insert_table db
                    ~name:name ~comment:comment ~logged
      in
        List.iter
          (fun c -> ignore (column_of_dtd_valid_xml table c))
          (Xml.children columns);
        begin
          match pkey with
            | None      -> ()
            | Some pkey ->
                let rec fetch = function
                  | [] ->
                      []
                  | Xml.Element (_, [], [Xml.PCData (c)]) :: tl ->
                      (SQL_db.column_by_name table c) :: (fetch tl)
                  | _ ->
                      Dbf_misc.ie ()    (* DTD *)
                in
                  SQL_db.set_primary_key table (fetch (Xml.children pkey))
        end;
        table
    with
      | SQL_db.Invalid_name _    -> raise Invalid_db_file
      | SQL_db.Duplicated_name _ -> raise Invalid_db_file
      | Not_found                -> raise Invalid_db_file

let index_of_dtd_valid_xml = fun db xml ->
  let name       = pcdata_from_child_tag xml "name"
  and tableref   = pcdata_from_child_tag xml "tableref"
  and columnsref = find_all_tags xml "columnref" in
    try
      let table   = SQL_db.table_by_name db tableref in
      let columns =
        List.map
          (fun c -> SQL_db.column_by_name table ~name:(pcdata_from_tag c))
          columnsref
      and unique  =
        db_bool_of_string (Xml.attrib xml "unique")
      in
        SQL_db.insert_index ~name:name ~columns:columns
          ~unique:unique
    with
      | Not_found                -> raise Invalid_db_file
      | SQL_db.Invalid_name _    -> raise Invalid_db_file
      | SQL_db.Invalid_args _    -> raise Invalid_db_file
      | SQL_db.Duplicated_name _ -> raise Invalid_db_file

let query_of_dtd_valid_xml = fun db xml ->
  let name       = pcdata_from_child_tag xml "name"
  and query      = pcdata_from_child_tag xml "querytext"
  and comment    = pcdata_from_child_tag xml "comment" in
  try
    SQL_db.insert_query db ~name ~query ~comment
  with
  | Not_found                -> raise Invalid_db_file
  | SQL_db.Invalid_name _    -> raise Invalid_db_file
  | SQL_db.Invalid_args _    -> raise Invalid_db_file
  | SQL_db.Duplicated_name _ -> raise Invalid_db_file

let column_fullref_of_dtd_valid_xml = fun db xml ->
  let tableref  = pcdata_from_child_tag xml "tableref"
  and columnref = pcdata_from_child_tag xml "columnref" in
    try
      let table  = SQL_db.table_by_name db tableref in
        SQL_db.column_by_name table columnref
    with
      | Not_found -> raise Invalid_db_file

let columneq_of_dtd_valid_xml = fun table xml ->
  let columnfullref = find_first_tag xml "columnfullref"
  and columnref     = pcdata_from_child_tag xml "columnref" in
    try
      let c1 = column_fullref_of_dtd_valid_xml
                 table.SQL_db.ta_db columnfullref
      and c2 = SQL_db.column_by_name table columnref in
        (c1, c2)
    with
      | Not_found -> raise Invalid_db_file

let join_of_dtd_valid_xml = fun db xml ->
  let tableref  = pcdata_from_child_tag xml "tableref"
  and columnseq = find_all_tags xml "columneq" in
    try
      let table = SQL_db.table_by_name db tableref in
      let ceq   =
        List.map (columneq_of_dtd_valid_xml table) columnseq
      in
        (table, ceq)
    with
      | Not_found -> raise Invalid_db_file

let vtable_of_dtd_valid_xml = fun db xml ->
  let name      = pcdata_from_child_tag xml "name"
  and ftableref = pcdata_from_child_tag xml "tableref"
  and joins     = find_all_tags xml "join" in
    try
      let ftable = SQL_db.table_by_name db ftableref in
      let vtable = SQL_db.create_vtable name ftable in
        List.iter
          (fun j -> let (table, ceq) = join_of_dtd_valid_xml db j in
             SQL_db.do_join vtable table ceq)
          joins;
        SQL_db.link_vtable_to_db vtable;
        vtable
    with
      | Not_found                -> raise Invalid_db_file
      | SQL_db.Invalid_name _    -> raise Invalid_db_file
      | SQL_db.Invalid_args _    -> raise Invalid_db_file
      | SQL_db.Duplicated_name _ -> raise Invalid_db_file

let db_of_xml = fun xml ->
  let xml =
    try  Dtd.prove checked_dtd "db" xml
    with
      | Dtd.Check_error _ -> raise Invalid_db_file
      | Dtd.Prove_error _ -> raise Invalid_db_file
  in
  let db = SQL_db.create_empty () in
  let tables  = find_first_tag xml "tables"
  and indexes = find_first_tag xml "indexes"
  and queries = find_first_tag xml "queries"
  and vtables = find_first_tag xml "vtables" in
    try
      List.iter
        (fun t -> ignore (table_of_dtd_valid_xml db t))
        (Xml.children tables);
      List.iter
        (fun i -> ignore (index_of_dtd_valid_xml db i))
        (Xml.children indexes);
      List.iter
        (fun q -> ignore (query_of_dtd_valid_xml db q))
        (Xml.children queries);
      List.iter
        (fun v -> ignore (vtable_of_dtd_valid_xml db v))
        (Xml.children vtables);
      db
    with
      | Xml.Not_pcdata _   -> Dbf_misc.ie () (* DTD *)
      | Xml.Not_element _  -> Dbf_misc.ie () (* DTD *)
      | Xml.No_attribute _ -> Dbf_misc.ie () (* DTD *)

let db_of_file = fun filename ->
  let xml =
    try
      Xml.parse_file filename
    with
    | Xml.Error _ -> raise Invalid_db_file
    | Xml.File_not_found _ -> raise Invalid_db_file (* XXX Use an another exception ? *)
  in
  db_of_xml xml
