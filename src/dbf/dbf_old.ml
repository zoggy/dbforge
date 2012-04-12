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

(* $Id: dbf_old.ml 758 2011-01-13 07:53:27Z zoggy $ *)

(** Types to describe an old DBForge schema. *)

(** Type of keys. *)
type t_key = Primary_key | Key

(** SQL code is a string. *)
type sql_code = string

    (** The various supported DBMS. *)
type dbms = Odbc | Mysql | Postgres

(** Column info for a specific dbms. *)
type column_dbms = {
    mutable col_type_sql : string * string option * string option ;
        (** SQL type, an optional argument, and optional args *)

    mutable col_2ml : string ;
        (** Name of the function to call to get a ml value from a string *)

    mutable col_ml2 : string ;
        (** Name of the function to call to get a strign from a ml value *)

    mutable col_key : t_key option ;
        (** optional key type *)

    mutable col_default : sql_code option ;
        (** optional default SQL value *)

    mutable col_atts : (string * sql_code) list ;
        (** list of (attribute name, SQL code for value) *)

  }

    (** A table column. *)
type column = {
    mutable col_name : string ;
         (** Name of the column, will also be the name
	    of the record in the ocaml record type of the table *)

    mutable col_comment : string;
        (** Comment of the column *)

    mutable col_type_ml : string;
        (** OCaml type to represent the SQL type *)

    mutable col_nullable : bool;
        (** column can contain NULL values or not *)

    mutable col_index : bool ;
        (** make an index on this column or not *)

    mutable col_dbms : (dbms * column_dbms) list ;
        (** DBMS-specific information *)

  }

(** A table. *)
type table = {
    mutable ta_name : string ;
    mutable ta_comment : string ;
    mutable ta_columns : column list ;
    mutable ta_atts : int list ; (** later, table attributes *)
    mutable ta_indexes : int list ; (** later, indexes on various columns *)
    mutable ta_logged : bool; (** whether this table must have a table_log *)
  }

(** A schema. *)
type schema = {
    mutable sch_tables : table list;
  }

(** {2 Reading old DBForge files} *)

let fail e s =
  failwith
    (Printf.sprintf "Bad old DBforge file (%s: %s)"
       s (Xml.to_string e)
    )

let bool_of_xml = function
    Xml.Element ("True",_,_) -> true
  | _ -> false

let unescape s =
  let s = Dbf_misc.strip_string s in
  let len = String.length s in
  if len < 2 then
    s
  else
    match s.[0], s.[len-1] with
      '"', '"' -> String.sub s 1 (len-2)
    | _ -> s

let db_of_xml = function
  | Xml.Element ("Postgres",_,_) -> Postgres
  | Xml.Element ("Mysql",_,_) -> Mysql
  | Xml.Element ("Odbc",_,_) -> Odbc
  | e -> fail e "db_of_xml"

let key_of_xml = function
  | Xml.Element("Primary_key",_,_) -> Primary_key
  | Xml.Element("Key",_,_) -> Key
  | e -> fail e "key_of_xml"

let opt_of_xml f = function
    Xml.Element ("None",_,[]) -> None
  | Xml.Element ("Some",_,[e]) -> Some  (f e)
  | e -> fail e "opt_of_xml"

let att_of_xml = function
    Xml.Element ("li",_,_) ->("","")
  | e -> fail e "att_of_xml"

let string_of_xml = function
  | Xml.PCData s -> unescape s
  | e -> fail e "string_of_xml"

let type_sql_of_xml = function
    [ Xml.Element("ti",_,[Xml.PCData t]);
      Xml.Element("ti",_,[s1]);
      Xml.Element("ti",_,[s2])
    ] ->
      (unescape t,
       opt_of_xml string_of_xml s1,
       opt_of_xml string_of_xml s2)
  | e -> fail (Xml.Element ("list",[],e)) "type_sql_of_xml"

let coldbms_of_xml = function
    Xml.Element
      ("column_dbms",_,
       [ Xml.Element ("col_type_sql",_,[Xml.Element("tuple",_,info)]) ;
         Xml.Element ("col_2ml",_,[Xml.PCData col2ml]);
         Xml.Element ("col_ml2",_,[Xml.PCData colml2]);
         Xml.Element ("col_key",_,[col_key]) ;
         Xml.Element ("col_default",_,[default]) ;
         Xml.Element ("col_atts",_,[Xml.Element("list",_,atts)])
       ]
      ) ->
        { col_type_sql = type_sql_of_xml info ;
          col_2ml = unescape col2ml ;
          col_ml2 = unescape colml2 ;
          col_key = opt_of_xml key_of_xml col_key ;
          col_default = opt_of_xml string_of_xml default;
          col_atts = List.map att_of_xml atts ;
        }
  | e -> fail e "coldbms_of_xml"

let dbms_of_xml = function
    Xml.Element
      ("li",_,
       [Xml.Element("tuple",_,
                [
                  Xml.Element ("ti",_,[db]) ;
                  Xml.Element ("ti",_,[col])
                ]
               )
       ]
      ) ->
        let db = db_of_xml db in
        let col = coldbms_of_xml col in
        (db, col)
  | e ->
      fail e "dbms_of_xml"

let column_of_xml = function
    Xml.Element
      ("li",_,
       [Xml.Element("column",_,
                [Xml.Element ("col_name",_,[Xml.PCData name]);
                  Xml.Element ("col_comment",_,[Xml.PCData comment]);
                  Xml.Element ("col_type_ml",_,[Xml.PCData type_ml]);
                  Xml.Element ("col_nullable",_,[nullable]);
                  Xml.Element ("col_index",_,[index]);
                  Xml.Element ("col_dbms",_,[Xml.Element("list",_,dbms)])
                ]
               )
       ]
      ) ->
        let dbms = List.map dbms_of_xml dbms in
        { col_name = unescape name ;
          col_comment = unescape comment ;
          col_type_ml = unescape type_ml ;
          col_nullable = bool_of_xml nullable ;
          col_index = bool_of_xml index ;
          col_dbms = dbms ;
        }
  | e -> fail e "column_of_xml"

let table_of_xml = function
    Xml.Element
      ("li",_,
       [Xml.Element("table",_,
                [Xml.Element ("ta_name",_,[Xml.PCData name]);
                  Xml.Element ("ta_comment",_,[Xml.PCData comment]);
                  Xml.Element ("ta_columns",_,[Xml.Element("list",_,cols)]);
                  Xml.Element ("ta_atts",_,[Xml.Element("list",_,atts)]);
                  Xml.Element ("ta_indexes",_,[Xml.Element("list",_,indexes)]);
                  Xml.Element ("ta_logged",_,[logged])
                ] )
       ]
      ) ->
        let columns = List.map column_of_xml cols in
        { ta_name = unescape name ;
          ta_comment = unescape comment ;
          ta_columns = columns ;
          ta_atts = [] ;
          ta_indexes = [] ;
          ta_logged = bool_of_xml logged ;
        }
  | e -> fail e "table_of_xml"

let schema_of_xml = function
    Xml.Element
      ("schema",_, [Xml.Element("sch_tables",_, [Xml.Element ("list",_,l)])]) ->
        { sch_tables = List.map table_of_xml l }
  | e -> fail e "schema_of_xml"

let read file =
  try
    let ic = open_in file in
    ignore(input_line ic);
    let xml = Xml.parse_in ic in
    close_in ic;
    schema_of_xml xml
  with
    Xml.Error (msg,loc) ->
      failwith (Printf.sprintf "file %s, line %d:\n%s" file
                  (Xml.line loc) (Xml.error_msg msg))

(** {2 Converting old to new format} *)

open Dbf_sql

let string_of_dbms = function
    Mysql -> "Mysql"
  | Postgres -> "Postgresql"
  | Odbc -> "ODBC"

let regular_type_of_old_sql_type t =
  let module T = SQL_db in
  let (s1,arg,_) = t in
  try
    let ty =
      match String.lowercase s1, arg with
        "tinyint", _ -> T.TinyInt (None,T.NO_None)
      | "mediumint", _ -> T.MediumInt (None,T.NO_None)
      | "int", _ -> T.Int (None,T.NO_None)
      | "bigint", _ -> T.BigInt (None, T.NO_None)
      | "double", _ -> T.Double (None, T.NO_None)
      | "float", _ -> T.Float (None, T.NO_None)
      | "decimal", _ -> T.Decimal (None,T.NO_None)
      | "char", None -> T.Char 1
      | "char", Some s
      | "char(m)", Some s -> T.Char (int_of_string s)
      | "varchar", Some s
      | "varchar(m)", Some s -> T.VarChar (int_of_string s)
      | "tinyblob", _ -> T.TinyBlob
      | "blob", _ -> T.Blob
      | "mediumblob", _ -> T.MediumBlob
      | "longblob", _ -> T.LongBlob
      | "tinytext", _ -> T.TinyText
      | "text", _ -> T.Text
      | "mediumtext", _ -> T.MediumText
      | "longtext", _ -> T.LongText
      | _, _ ->raise Not_found
    in
    Some ty
  with
    Not_found -> None

let sqltype_of_col dbms cdbms =
  match regular_type_of_old_sql_type cdbms.col_type_sql with
    Some t -> (t, Dbf_misc.StringMap.empty)
  | None ->
      let set = Dbf_misc.StringMap.empty in
      let set =
        let db = string_of_dbms dbms in
        let s =
          match cdbms.col_type_sql with
            s,None,_ -> s
          | s,Some s2,_ -> Printf.sprintf "%s(%s)" s s2
        in
        Dbf_misc.StringMap.add db s set
      in
      (SQL_db.Int (None,SQL_db.NO_None), set)

let convert_column table dbms col =
  let cdbms = List.assoc dbms col.col_dbms in
  let ty,spec_tys = sqltype_of_col dbms cdbms in
  let c = SQL_db.insert_column table
      ~name: col.col_name
      ~comment: col.col_comment
      ~ty
      ~nullable: col.col_nullable
      ()
  in
  c.SQL_db.col_ocaml_ty <- col.col_type_ml;
  c.SQL_db.col_sql2ml <- cdbms.col_2ml;
  c.SQL_db.col_ml2sql <- cdbms.col_ml2;
  c.SQL_db.col_spec_ty <- spec_tys ;

  let spec_options =
    let empty = Dbf_misc.StringMap.empty in
    match cdbms.col_type_sql with
      (_,_,None) ->  empty
    | (_,_,Some s) ->
        let db = string_of_dbms dbms in
        Dbf_misc.StringMap.add db [s] empty
  in
  c.SQL_db.col_spec_options <- spec_options


let convert_table db dbms old_table =
  let table = SQL_db.insert_table db
      ~name:old_table.ta_name ~comment:old_table.ta_comment
      ~logged: old_table.ta_logged
  in
  List.iter (convert_column table dbms) old_table.ta_columns;
  let keys = List.filter
      (fun c ->
        let cdbms = List.assoc dbms c.col_dbms in
        cdbms.col_key=Some Primary_key
      )
      old_table.ta_columns
  in
  let pkey = List.map
      (fun c -> SQL_db.column_by_name table c.col_name)
      keys
  in
  SQL_db.set_primary_key table pkey

let convert_to_db old dbms =
  let db = SQL_db.create_empty () in
  List.iter (convert_table db dbms) old.sch_tables;
  db
