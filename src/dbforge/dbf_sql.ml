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

(*==========\
| Interface |
\==========*)
module type SQL_db = sig
  type numeric_option = NO_None | NO_Unsigned | NO_UnsignedZeroFill

  type ty =
    | TinyInt   of int option * numeric_option
    | MediumInt of int option * numeric_option
    | Int       of int option * numeric_option
    | BigInt    of int option * numeric_option

    | Double    of (int * int) option * numeric_option
    | Float     of (int * int) option * numeric_option
    | Decimal   of (int * int) option * numeric_option

    | Char      of int
    | VarChar   of int

    | TinyBlob
    | Blob
    | MediumBlob
    | LongBlob

    | TinyText
    | Text
    | MediumText
    | LongText

  type table = {
      mutable ta_name    : string;
      mutable ta_comment : string;
      mutable ta_pkey    : column list;
      mutable ta_db      : db;
      mutable ta_columns : column list;
      mutable ta_logged  : bool ;
    }

  and vtable = {
      mutable vt_name   : string;
      mutable vt_db     : db;
      mutable vt_ftable : table;
      mutable vt_join   : (table * (column * column) list) list
    }

  and column = {
      mutable col_name         : string;
      mutable col_comment      : string;
      mutable col_table        : table;
      mutable col_type         : ty;
      mutable col_nullable     : bool;
      mutable col_spec_options : (string list) Dbf_misc.StringMap.t;
      mutable col_spec_ty      : string Dbf_misc.StringMap.t;
      mutable col_ocaml_ty     : string;
      mutable col_sql2ml       : string;
      mutable col_ml2sql       : string;
    }

  and index = {
      mutable idx_name     : string;
      mutable idx_columns  : column list;
      mutable idx_unique   : bool;
      mutable idx_db       : db;
    }

  and query = {
      mutable qry_name    : string ;
      mutable qry_query   : string ;
      mutable qry_comment : string ;
      mutable qry_db      : db;
    }

  and db = {
      mutable db_tables  : table list;
      mutable db_vtables : vtable list;
      mutable db_indexes : index list;
      mutable db_queries : query list;
    }

  exception Duplicated_name of string
  exception Invalid_name    of string
  exception Invalid_args    of string

  val validate_name     : string -> bool
  val validate_name_exn : string -> unit
  val create_empty      : unit -> db

    (* Functions on tables *)
  val create_table_name : db -> ?prefix: string -> ?from: int -> unit -> string
  val table_by_name     : db -> string -> table
  val table_by_name_opt : db -> string -> table option

  val insert_table : db -> name: string -> comment: string -> logged: bool -> table
  val unlink_table : table -> (vtable list * index list)
  val rename_table : table -> name: string -> unit

  val set_primary_key   : table -> column list -> unit
  val unset_primary_key : table -> unit

  (* Functions on columns *)
  val column_fullname : column -> string

  val create_column_name : table -> ?prefix: string -> ?from: int -> unit -> string
  val column_by_name     : table -> name: string -> column
  val column_by_name_opt : table -> name: string -> column option

  val insert_column : table ->
    name: string    ->
      comment: string ->
        ty: ty          ->
          ?nullable: bool ->
            unit -> column

  val rename_column : column -> name: string -> unit
  val unlink_column : column -> (vtable list * index list * bool)

  val string_of_spec_options : (string list) Dbf_misc.StringMap.t -> string

  (* Function on virtual tables - FIXME: better interface needed *)
  val create_vtable_name : db -> ?prefix: string -> ?from: int -> unit -> string
  val vtable_by_name     : db -> string -> vtable
  val vtable_by_name_opt : db -> string -> vtable option

  val create_vtable      : name: string -> table: table -> vtable
  val link_vtable_to_db  : vtable -> unit
  val do_join            : vtable -> table -> (column * column) list -> unit

  val table_in_join      : vtable -> table -> bool

  val rename_vtable      : vtable -> name: string -> unit
  val unlink_vtable      : vtable -> unit

  val string_of_vtable   : vtable -> string

  val vtables_using_table  : table  -> vtable list
  val vtables_using_column : column -> vtable list

  (*  Function on indexes *)
  val create_index_name : db -> ?prefix: string -> ?from: int -> unit -> string
  val index_by_name     : db -> string -> index
  val index_by_name_opt : db -> string -> index option

  val insert_index      : name: string -> columns: column list -> unique: bool -> index
  val rename_index      : index -> name: string -> unit
  val unlink_index      : index -> unit

  val column_in_index   : index -> column -> bool

  val string_of_index   : index -> string
  val table_of_index    : index -> table

  val update_index      : index -> name:string ->
    columns:(column list) -> unique:bool ->
      unit

  val indexes_using_table  : table  -> index list
  val indexes_using_column : column -> index list

  (*  Function on queries *)

  type query_state =
    | Query_ok of (column option) list (* return values (column option) *)
	  * (string * column option) list (* ok, parameters (name,column option) *)
    | Query_parse_error of int * int * string (* line, char, message *)
    | Query_invalid_against_schema of string
    | Query_incorrect of string

  val create_query_name : db -> ?prefix: string -> ?from: int -> unit -> string
  val query_by_name     : db -> string -> query
  val query_by_name_opt : db -> string -> query option

  val insert_query      : db -> name: string -> query: string -> comment: string -> query
  val rename_query      : query -> name: string -> unit
  val unlink_query      : query -> unit

  val update_query      : query -> name:string ->
    query: string -> comment: string -> unit

  val query_state : query -> query_state
  val string_of_query_state : query_state -> string
end

(*==============\
| Implmentation |
\==============*)
module SQL_db: SQL_db = struct
  type numeric_option = NO_None | NO_Unsigned | NO_UnsignedZeroFill
  type char_option    = CO_None | CO_Binary | CO_Ascii
  type varchar_option = VCO_None | VCO_Binary

  type ty =
    | TinyInt   of int option * numeric_option
    | MediumInt of int option * numeric_option
    | Int       of int option * numeric_option
    | BigInt    of int option * numeric_option

    | Double    of (int * int) option * numeric_option
    | Float     of (int * int) option * numeric_option
    | Decimal   of (int * int) option * numeric_option

    | Char      of int
    | VarChar   of int

    | TinyBlob
    | Blob
    | MediumBlob
    | LongBlob

    | TinyText
    | Text
    | MediumText
    | LongText

  type table = {
    mutable ta_name    : string;
    mutable ta_comment : string;
    mutable ta_pkey    : column list;
    mutable ta_db      : db;
    mutable ta_columns : column list;
    mutable ta_logged  : bool ;
  }

  and vtable = {
    mutable vt_name   : string;
    mutable vt_db     : db;
    mutable vt_ftable : table;
    mutable vt_join   : (table * (column * column) list) list
  }

  and column = {
    mutable col_name         : string;
    mutable col_comment      : string;
    mutable col_table        : table;
    mutable col_type         : ty;
    mutable col_nullable     : bool;
    mutable col_spec_options : (string list) Dbf_misc.StringMap.t;
    mutable col_spec_ty      : string Dbf_misc.StringMap.t;
    mutable col_ocaml_ty     : string;
    mutable col_sql2ml       : string;
    mutable col_ml2sql       : string;
  }

  and index = {
    mutable idx_name     : string;
    mutable idx_columns  : column list;
    mutable idx_unique   : bool;
    mutable idx_db       : db;
  }

  and query = {
      mutable qry_name    : string ;
      mutable qry_query   : string ;
      mutable qry_comment : string ;
      mutable qry_db      : db;
    }

  and db = {
    mutable db_tables  : table list;
    mutable db_vtables : vtable list;
    mutable db_indexes : index list;
    mutable db_queries : query list;
  }

  exception Invalid_name    of string
  exception Duplicated_name of string
  exception Invalid_args    of string

  let validate_name = fun s ->
    Str.string_match (Str.regexp "^[a-zA-Z0-9_]+$") s 0

  let validate_name_exn = fun s ->
    if not (validate_name s) then
      raise (Invalid_name s)

  let create_empty = fun () ->
    { db_tables = []; db_vtables = [];
      db_indexes = []; db_queries = []; }

  (*=======\
  | Tables |
  \=======*)
  let table_by_name = fun db name ->
    List.find (fun t -> t.ta_name = name) db.db_tables

  let table_by_name_opt = fun db name ->
    try
      Some (table_by_name db name)
    with
      | Not_found -> None

  let create_table_name = fun db ?(prefix = "table_") ?(from = 0) () ->
    let rec create = fun idx ->
      let name = prefix ^ (string_of_int idx) in
        match table_by_name_opt db name with
          | None   -> name
          | Some _ -> create (idx + 1)
    in
      create from

  let insert_table = fun db ~name ~comment ~logged ->
    validate_name_exn name;
    match table_by_name_opt db name with
      | None -> let table = { ta_name    = name;
                              ta_comment = comment;
                              ta_db      = db;
                              ta_columns = [];
                              ta_logged  = logged ;
                              ta_pkey    = []; }
        in
          db.db_tables <- db.db_tables @ [table];
          table
      | Some _ ->
          raise (Duplicated_name name)

  let rename_table = fun table ~name ->
    if table.ta_name <> name then begin
      validate_name_exn name;
      match table_by_name_opt table.ta_db name with
        | None   -> table.ta_name <- name
        | Some _ -> raise (Duplicated_name name)
    end

  let set_primary_key = fun table columns ->
    assert (List.for_all                (* Phys. eq *)
              (fun c -> c.col_table == table)
              columns);
    table.ta_pkey <- columns

  let unset_primary_key = fun table -> table.ta_pkey <- []

  (*========\
  | Columns |
  \========*)
  let column_fullname = fun c ->
    Printf.sprintf "%s.%s" c.col_table.ta_name c.col_name

  let column_by_name = fun table ~name ->
    List.find (fun c -> c.col_name = name) table.ta_columns

  let column_by_name_opt = fun table ~name ->
    try
      Some (column_by_name table name)
    with
      | Not_found -> None

  let create_column_name = fun table ?(prefix = "column_") ?(from = 0) () ->
    let rec create = fun idx ->
      let name = prefix ^ (string_of_int idx) in
        match column_by_name_opt table name with
          | None   -> name
          | Some _ -> create (idx + 1)
    in
      create from

  let insert_column = fun table ~name ~comment ~ty ?(nullable = true) () ->
    validate_name_exn name;
    match column_by_name_opt table name with
      | None -> let column = { col_name         = name;
                               col_comment      = comment;
                               col_table        = table;
                               col_type         = ty;
                               col_nullable     = nullable;
                               col_spec_options = Dbf_misc.StringMap.empty;
                               col_spec_ty      = Dbf_misc.StringMap.empty;
                               col_ocaml_ty     = "";
                               col_sql2ml       = "";
                               col_ml2sql       = ""; }
        in
          table.ta_columns <- table.ta_columns @ [column];
          column
      | Some _ ->
          raise (Duplicated_name name)

  let rename_column = fun column ~name ->
    if column.col_name <> name then begin
      validate_name_exn name;
      match column_by_name_opt column.col_table name with
        | None   -> column.col_name <- name
        | Some _ -> raise (Duplicated_name name)
    end

  let string_of_spec_options = fun opts ->
    let string_of_spec_options_db = fun db options ->
      Printf.sprintf "%s = [%s]"  db
        (Dbf_misc.join ~sep:", " ~to_string:(fun x -> x) options)
    in
    let strings =
      Dbf_misc.StringMap.fold
        (fun db options strings ->
           (string_of_spec_options_db db options) :: strings)
        opts
        []
    in
      Dbf_misc.join ~sep:", " ~to_string:(fun x -> x)
        (List.rev strings)

  (*===============\
  | Virtual tables |
  \===============*)
  let create_vtable = fun ~name ~table ->
    validate_name_exn name;
    { vt_name   = name;
      vt_ftable = table;
      vt_join   = [];
      vt_db     = table.ta_db; }

  let do_join = fun vtable table constraints ->
    assert (vtable.vt_db == table.ta_db); (* Physical eq *)
    (* FIXME: We should check the constraints *)
    vtable.vt_join <- vtable.vt_join @ [table, constraints]

  let table_in_join = fun vtable table ->
    assert (vtable.vt_db == table.ta_db); (* Physical eq *)
    if vtable.vt_ftable == table then
      true
    else
      List.exists (fun (t, _) -> t == table) vtable.vt_join

  let vtable_by_name = fun db name ->
    List.find (fun vt -> vt.vt_name = name) db.db_vtables

  let vtable_by_name_opt = fun db name ->
    try
      Some (List.find (fun vt -> vt.vt_name = name) db.db_vtables)
    with
      | Not_found -> None

  let create_vtable_name = fun db ?(prefix = "vtable_") ?(from = 0) () ->
    let rec create = fun idx ->
      let name = prefix ^ (string_of_int idx) in
        match vtable_by_name_opt db name with
          | None   -> name
          | Some _ -> create (idx + 1)
    in
      create from

  let link_vtable_to_db = fun vtable ->
    let db = vtable.vt_db in
      List.iter
        (fun vt -> assert (vt != vtable); (* Physical eq *)
           if vtable.vt_name = vt.vt_name then
             raise (Duplicated_name vtable.vt_name))
        db.db_vtables;
      db.db_vtables <- db.db_vtables @ [vtable]

  let unlink_vtable = fun vtable ->
    let (vtables, to_be_removed) =
      List.partition (fun vt -> vt.vt_name <> vtable.vt_name)
        vtable.vt_db.db_vtables
    in
      vtable.vt_db.db_vtables <- vtables;
      match to_be_removed with
        | [vt] -> vt.vt_db <- (Obj.magic 0)
        | _    -> Dbf_misc.ie ()

  let rename_vtable = fun vtable ~name ->
    if name <> vtable.vt_name then
      match vtable_by_name_opt vtable.vt_db name with
        | None   -> vtable.vt_name <- name
        | Some _ -> raise (Duplicated_name name)

  (* FIXME: beurk, beurk *)
  let string_of_vtable = fun vtable ->
    let current = ref vtable.vt_ftable.ta_name
    and first   = ref true
    and string_of_constraint = fun (c1, c2) ->
      Printf.sprintf "%s = %s"
      (column_fullname c1)
      (column_fullname c2) in
    let do_join = fun (table, columns) ->
      let parent_current =
        if   !first
        then !current
        else Printf.sprintf "(%s)" !current
      in
        first := false;
        match columns with
          | [] ->
              current := Printf.sprintf "%s, %s" parent_current table.ta_name
          | _ ->
              let constraints =
                Dbf_misc.join
                  ~sep:" AND "
                  ~to_string:string_of_constraint
                  columns
              in
                current :=
                Printf.sprintf "%s INNER JOIN %s ON %s"
                  parent_current table.ta_name constraints
    in
      List.iter do_join vtable.vt_join;
      !current

  (*========\
  | Indexes |
  \========*)
  let table_of_index = fun index ->
    match index.idx_columns with
      | []      -> Dbf_misc.ie ()
      | hd :: _ -> hd.col_table

  let string_of_index = fun index ->
    Dbf_misc.join ~sep:", " ~to_string:(fun c -> c.col_name)
      index.idx_columns

  let index_by_name = fun db name ->
    List.find (fun i -> i.idx_name = name) db.db_indexes

  let index_by_name_opt = fun db name ->
    try
      Some (List.find (fun i -> i.idx_name = name) db.db_indexes)
    with
      | Not_found -> None

  let column_in_index = fun index column ->
    (* Physical eq *)
    List.memq column index.idx_columns

  let unlink_index = fun index ->
    let (indexes, to_be_removed) =
      List.partition (fun idx -> idx.idx_name <> index.idx_name)
        index.idx_db.db_indexes
    in
      index.idx_db.db_indexes <- indexes;
      match to_be_removed with
        | [idx] -> idx.idx_db <- (Obj.magic 0)
        | _     -> Dbf_misc.ie ()

  let create_index_name = fun db ?(prefix = "index_") ?(from = 0) () ->
    let rec create = fun idx ->
      let name = prefix ^ (string_of_int idx) in
        match index_by_name_opt db name with
          | None   -> name
          | Some _ -> create (idx + 1)
    in
      create from

  let insert_index = fun ~name ~columns ~unique ->
    validate_name_exn name;
    match columns with
      | []       ->
          raise (Invalid_args "An index must contain at least one column")
      | hd :: _  ->
          let db = hd.col_table.ta_db in
            assert
              (not (List.exists
                      (fun c -> c.col_table.ta_db != db) (* Phys. eq *)
                      columns));
            if index_by_name_opt db name <> None then
              raise (Duplicated_name name);
            let index = { idx_name    = name;
                          idx_db      = db;
                          idx_columns = columns;
                          idx_unique  = unique; }
            in
              db.db_indexes <- db.db_indexes @ [index];
              index

  let rename_index = fun index ~name ->
    if index.idx_name <> name then begin
      validate_name_exn name;
      match index_by_name_opt index.idx_db name with
        | None   -> index.idx_name <- name
        | Some _ -> raise (Duplicated_name name)
    end

  let update_index = fun index ~name ~columns ~unique ->
    if index.idx_name <> name then begin
      validate_name_exn name;
      begin
        match index_by_name_opt index.idx_db name with
          | None   -> index.idx_name <- name
          | Some _ -> raise (Duplicated_name name)
      end;
    end;
    if columns = [] then
      raise (Invalid_args "An index must contain at least one column");
    index.idx_name <- name;
    index.idx_columns <- columns;
    index.idx_unique <- unique

  (*========\
  | Queries |
  \========*)

  type query_state =
    | Query_ok of (column option) list (* return values (column option) *)
	  * (string * column option) list (* ok, parameters (name,column option) *)
    | Query_parse_error of int * int * string (* line, char, message *)
    | Query_invalid_against_schema of string
    | Query_incorrect of string

  exception Bad_query of query_state

  let query_by_name = fun db name ->
    List.find (fun q -> q.qry_name = name) db.db_queries

  let query_by_name_opt = fun db name ->
    try Some (query_by_name db name)
    with Not_found -> None

  let unlink_query = fun query ->
    let (queries, to_be_removed) =
      List.partition (fun q -> q.qry_name <> q.qry_name)
        query.qry_db.db_queries
    in
    query.qry_db.db_queries <- queries;
    match to_be_removed with
    | [q] -> q.qry_db <- (Obj.magic 0)
    | _   -> Dbf_misc.ie ()

  let create_query_name = fun db ?(prefix = "query_") ?(from = 0) () ->
    let rec create = fun idx ->
      let name = prefix ^ (string_of_int idx) in
        match query_by_name_opt db name with
          | None   -> name
          | Some _ -> create (idx + 1)
    in
    create from

  let insert_query = fun db ~name ~query ~comment ->
    validate_name_exn name;
    if query_by_name_opt db name <> None then
      raise (Duplicated_name name);
    let query = { qry_name    = name;
                  qry_db      = db;
                  qry_query   = query;
                  qry_comment = comment; }
    in
    db.db_queries <- db.db_queries @ [query];
    query

  let rename_query = fun query ~name ->
    if query.qry_name <> name then begin
      validate_name_exn name;
      match query_by_name_opt query.qry_db name with
        | None   -> query.qry_name <- name
        | Some _ -> raise (Duplicated_name name)
    end

  let update_query = fun q ~name ~query ~comment ->
    if q.qry_name <> name then begin
      validate_name_exn name;
      begin
        match query_by_name_opt q.qry_db name with
          | None   -> q.qry_name <- name
          | Some _ -> raise (Duplicated_name name)
      end;
    end;
    q.qry_query <- query;
    q.qry_comment <- comment

  let query_parameters db query =
    let params = Hashtbl.create 13 in
    let bad state = raise (Bad_query state) in
    let add_param name colopt =
      try
	ignore (Hashtbl.find params name)
	(* FIXME: check that types/colums are identical ? *)
      with
	Not_found ->
	  Hashtbl.add params name colopt
    in
    let table_by_name s =
      try table_by_name db s
      with Not_found ->
	let msg = Printf.sprintf "No table %s in schema" s in
	bad (Query_invalid_against_schema msg)
    in

    let env_from_from =
      let f acc t =
	let (name,t) = match t with
	  `table s -> (s, table_by_name s)
	| `tableas (s1,s2) ->
	    let t = table_by_name s1 in
	    (s2, t)
	in
	if List.exists (fun (s,_) -> s = name) acc then
	  let msg = Printf.sprintf
	      "No unique table name %s in from clause" name
	  in
	  bad (Query_incorrect msg)
	else
	  (name,t) :: acc
      in
      List.fold_left f
    in
    let get_column env = function
	`ref s ->
	  let l = List.fold_left
	      (fun acc (table,t) ->
		match column_by_name_opt t s with
		  None -> acc
		| Some c -> (table,c)::acc)
	      []
	      env
	  in
	  (
	   match l with
	     [] ->
	       let msg = Printf.sprintf "Unbound column name %s" s in
	       bad (Query_invalid_against_schema msg)
	   | [(_,c)] -> c
	   | _ :: _ :: _ ->
	       let msg = Printf.sprintf
		   "Ambiguous column name %s; could belong to %s"
		   s (String.concat ", " (List.map fst l))
	       in
	       bad (Query_invalid_against_schema msg)
	  )
      |	`refdotref (s1,s2) ->
	  try
	    let t = List.assoc s1 env in
	    column_by_name t s2
	  with
	    Not_found ->
	      let msg = Printf.sprintf "Unbound column name %s.%s" s1 s2 in
	      bad (Query_invalid_against_schema msg)
    in
    let check_column env c = ignore (get_column env c) in
    let rec in_query_exp env = function
	`select s -> in_select env s
      |	`union (q1,q2) ->
	  (in_query_exp env q1) @ (in_query_exp env q2)
      |	`unionall (q1,q2) ->
	  (in_query_exp env q1) @ (in_query_exp env q2)
    and in_select env (_mod,selection,from,where_opt,group_by_opt,having_opt) =
      let env = env_from_from env from in
      let return_type =
	match selection with
	  `star -> bad (Query_incorrect "'*' not supported in selection")
	| `list l ->
	    let f = function
		`column c -> Some (get_column env c)
	      |	_ -> None
	    in
	    List.map f l
      in
      (match where_opt with None -> () | Some w -> in_condition env w);
      (match group_by_opt with None -> () | Some g -> in_group_by env g);
      (match having_opt with None -> () | Some h -> in_condition env h);
      return_type
    and in_group_by env l = List.iter (check_column env) l
    and in_condition env = function
	`cand (c1, c2)
      |	`cor (c1, c2) -> in_condition env c1; in_condition env c2
      |	`cnot c -> in_condition env c
      |	`p p -> in_predicate env p
    (* FIXME: improve analysis of expressions to guess types of parameters *)
    and in_predicate env = function
	`comparisonexp (e1,_,e2) -> in_exp env e1; in_exp env e2
      |	`comparisonselect (e1,_,sel) ->
	  in_exp env e1; ignore (in_select env sel)
      |	`between (_,e1,e2,e3) ->
	  in_exp env e1; in_exp env e2; in_exp env e3
      |	`like (_,e1,a1,a2opt) ->
	  in_exp env e1;
	  in_atom env a1;
	  (match a2opt with None -> () | Some a -> in_atom env a)
      |	`iscolnull (_,col) -> check_column env col
      |	`in_select (_,e1,sel) ->
	  in_exp env e1; ignore (in_select env sel)
      |	`in_atom_list (_,e1,l) ->
	  in_exp env e1; List.iter (in_atom env) l
      |	`allorany (e1,_,_,sel) ->
	  in_exp env e1; ignore (in_select env sel)
      |	`exists sel -> ignore (in_select env sel)
    and in_exp env = function
	`binop (_,e1,e2) ->
	  in_exp env e1; in_exp env e2
      |	`uminus e -> in_exp env e
      |	`atom a -> in_atom env a
      |	`column c -> check_column env c
      |	`functioncall _ ->
	  (* FIXME: implement *)
	  ()
    and in_atom env = function
	`parameter p -> in_parameter env p
      |	_ -> ()
    and in_parameter env = function
	`single s -> add_param s None
      |	`single_annotated (s,s2,s3) ->
	  (
	   try
	     let t = table_by_name s2 in
	     let col = column_by_name t s3 in
	     add_param s (Some col)
	   with
	     Not_found ->
	       let msg = Printf.sprintf "Unknown column %s.%s" s2 s3 in
	       bad (Query_invalid_against_schema msg)
	  )
      |	`couple _ ->
	  bad (Query_incorrect "Don't know what to do with couple parameters")
      |	`indicator _ ->
	  bad (Query_incorrect "Don't know what to do with indicator parameters")
    in
    let (qexp,ordlist) = query in
    let return_type = in_query_exp [] qexp in
    (* FIXME: handle ordering list *)
    (* FIXME: prevent parsing of capitalized parameters ? *)
    let params =
      Hashtbl.fold
	(fun name colopt acc -> (name,colopt)::acc)
	params
	[]
    in
    let params = List.sort
	(fun (name1,_) (name2,_) -> Pervasives.compare name1 name2)
	params
    in
    (return_type, params)


  let query_state = fun query ->
    try
      let q = Sqml.query_of_string query.qry_query in
      let (t,l) = query_parameters query.qry_db q in
      Query_ok (t,l)
    with
      Sqml.Syntax_error (l,c,s) -> Query_parse_error (l,c,s)
    | Bad_query s -> s

  let string_of_query_state = function
    | Query_invalid_against_schema s -> s
    | Query_incorrect s -> s
    | Query_parse_error (l,c,s) ->
	Printf.sprintf "%s line %d character %d" s l c
    | Query_ok (t,params) ->
	let type_of_col_opt = function
	    None -> "string option"
	  | Some col ->
	      Printf.sprintf "(%s%s)"
		col.col_ocaml_ty
		(if col.col_nullable then " option" else "")
	in
	let params = List.map
	    (fun (name,copt) ->
	      Printf.sprintf "%s: %s -> " name
		(type_of_col_opt copt)
	    )
	    params
	in
	let t = Printf.sprintf "(%s) list"
	    (String.concat "*" (List.map type_of_col_opt t))
	in
	Printf.sprintf "%s %s" (String.concat "" params) t

  (*======================================\
  | Table/Virtual table/Index interaction |
  \======================================*)
  let vtables_using_table_part = fun table ->
    let vtable_use_table = fun vtable ->
      vtable.vt_ftable == table ||      (* Phys. eq *)
      List.exists (fun (t, _) -> t == table) vtable.vt_join
    in
      List.partition vtable_use_table table.ta_db.db_vtables

  let vtables_using_column_part = fun c ->
    let db = c.col_table.ta_db
    and vtable_use_column = fun vtable ->
      List.exists
        (fun (_, columns) ->
           List.exists (fun (c1, c2) -> c1 == c || c2 == c) columns)
        vtable.vt_join
    in
      List.partition vtable_use_column db.db_vtables

  let indexes_using_table_part = fun table ->
    let index_use_table = fun index ->
      match index.idx_columns with
        | c :: _ -> c.col_table == table (* Phys. eq *)
        | _      -> Dbf_misc.ie ()
    in
      List.partition index_use_table table.ta_db.db_indexes

  let indexes_using_column_part = fun column ->
    let index_use_column = fun index ->
      List.exists (fun c -> c == column) index.idx_columns (* Phys. eq *)
    in
      List.partition index_use_column
        column.col_table.ta_db.db_indexes

  let vtables_using_table = fun table ->
    fst (vtables_using_table_part table)

  let vtables_using_column = fun c ->
    fst (vtables_using_column_part c)

  let indexes_using_table = fun table ->
    fst (indexes_using_table_part table)

  let indexes_using_column = fun column ->
    fst (indexes_using_column_part column)

  (*=======\
  | tables |
  \=======*)
  let unlink_table = fun table ->
    let (tables, to_be_removed) =
      List.partition (fun t -> t.ta_name <> table.ta_name)
        table.ta_db.db_tables
    in
    let (t_use, t_dont_use) = vtables_using_table_part table
    and (i_use, i_dont_use) = indexes_using_table_part table in
      table.ta_db.db_tables  <- tables;
      table.ta_db.db_vtables <- t_dont_use;
      table.ta_db.db_indexes <- i_dont_use;
      match to_be_removed with
        | [t] -> t.ta_db <- create_empty (); (t_use, i_use)
        | _   -> Dbf_misc.ie ()

  let unlink_column = fun column ->
    let (columns, to_be_removed) =
      List.partition (fun c -> c.col_name <> column.col_name)
        column.col_table.ta_columns
    in
    let (t_use, t_dont_use)   = vtables_using_column_part column
    and (i_use, i_dont_use)   = indexes_using_column_part column
    and (pk_use, pk_dont_use) =
      (* Phys. eq *)
      List.partition (fun c -> c == column) column.col_table.ta_pkey
    in
      column.col_table.ta_columns       <- columns;
      column.col_table.ta_pkey          <- pk_dont_use;
      column.col_table.ta_db.db_vtables <- t_dont_use;
      column.col_table.ta_db.db_indexes <- i_dont_use;
      match to_be_removed with
        | [c] -> c.col_table <- (Obj.magic 0); (t_use, i_use, pk_use <> [])
        | _   -> Dbf_misc.ie ()
end

(*==========\
| Interface |
\==========*)
module type SQL_ty =
sig
  type ty_kind =
    | SQL_TinyInt | SQL_MediumInt | SQL_Int | SQL_BigInt
    | SQL_Double  | SQL_Float     | SQL_Decimal
    | SQL_Char    | SQL_VarChar
    | SQL_TinyBlob | SQL_Blob | SQL_MediumBlob | SQL_LongBlob
    | SQL_TinyText | SQL_Text | SQL_MediumText | SQL_LongText

  type ty_class =
    | SQL_C_Int | SQL_C_Real  | SQL_C_Char

  exception Invalid_type of string

  val kind_of_type : SQL_db.ty -> ty_kind

  val kind_uses_display_width : ty_kind -> Dbf_misc.yes_no_maybe
  val kind_uses_precision     : ty_kind -> Dbf_misc.yes_no_maybe
  val options_of_kind         : ty_kind -> string list

  val kind_string_assoc        : (ty_kind * string) list
  val numeric_opt_string_assoc : (SQL_db.numeric_option * string) list

  val string_of_kind : ty_kind -> string
  val kind_of_string : string  -> ty_kind

  val class_of_kind  : ty_kind -> ty_class

  val string_of_numeric_option : SQL_db.numeric_option -> string
  val numeric_option_of_string : string -> SQL_db.numeric_option

  val get_display_size     : SQL_db.ty -> int option
  val get_precision        : SQL_db.ty -> int option
  val get_options_as_string : SQL_db.ty -> string option

  val type_of_string :
       ?dispsize: int
    -> ?precision: int
    -> ?options: string
    -> string -> SQL_db.ty

  val string_of_type         : SQL_db.ty -> string
  val string_of_type_options : SQL_db.ty -> string option
  val fullstring_of_type     : SQL_db.ty -> string

end

(*===============\
| Implementation |
\===============*)
module SQL_ty: SQL_ty =
struct
  open Dbf_misc

  type ty = SQL_db.ty

  type numeric_option = SQL_db.numeric_option

  type ty_kind =
    | SQL_TinyInt | SQL_MediumInt | SQL_Int | SQL_BigInt
    | SQL_Double | SQL_Float | SQL_Decimal
    | SQL_Char | SQL_VarChar
    | SQL_TinyBlob | SQL_Blob | SQL_MediumBlob | SQL_LongBlob
    | SQL_TinyText | SQL_Text | SQL_MediumText | SQL_LongText

  type ty_class =
    | SQL_C_Int | SQL_C_Real  | SQL_C_Char

  exception Invalid_type of string

  let kind_string_assoc =
    [(SQL_TinyInt,    "TINYINT");
     (SQL_MediumInt,  "MEDIUMINT");
     (SQL_Int,        "INT");
     (SQL_BigInt,     "BIGINT");
     (SQL_Double,     "DOUBLE");
     (SQL_Float,      "FLOAT");
     (SQL_Decimal,    "DECIMAL");
     (SQL_Char,       "CHAR");
     (SQL_VarChar,    "VARCHAR");
     (SQL_TinyBlob,   "TINYBLOB");
     (SQL_Blob,       "BLOB");
     (SQL_MediumBlob, "MEDIUMBLOB");
     (SQL_LongBlob,   "LONGBLOB");
     (SQL_TinyText,   "TINYTEXT");
     (SQL_Text,       "TEXT");
     (SQL_MediumText, "MEDIUMTEXT");
     (SQL_LongText,   "LONGTEXT")]

  let numeric_opt_string_assoc =
    [(SQL_db.NO_None,             "");
     (SQL_db.NO_Unsigned,         "UNSIGNED");
     (SQL_db.NO_UnsignedZeroFill, "UNSIGNED ZEROFILL")]

  let kind_of_type = function
    | SQL_db.TinyInt   _ -> SQL_TinyInt
    | SQL_db.MediumInt _ -> SQL_MediumInt
    | SQL_db.Int _       -> SQL_Int
    | SQL_db.BigInt _    -> SQL_BigInt

    | SQL_db.Double _  -> SQL_Double
    | SQL_db.Float _   -> SQL_Float
    | SQL_db.Decimal _ -> SQL_Decimal

    | SQL_db.Char _    -> SQL_Char
    | SQL_db.VarChar _ -> SQL_VarChar

    | SQL_db.TinyBlob   -> SQL_TinyBlob
    | SQL_db.Blob       -> SQL_Blob
    | SQL_db.MediumBlob -> SQL_MediumBlob
    | SQL_db.LongBlob   -> SQL_LongBlob

    | SQL_db.TinyText   -> SQL_TinyText
    | SQL_db.Text       -> SQL_Text
    | SQL_db.MediumText -> SQL_MediumText
    | SQL_db.LongText   -> SQL_LongText

  let string_of_kind =
    fun ty -> List.assoc ty kind_string_assoc

  let kind_of_string = fun s ->
    fst (List.find (fun (_, s') -> s = s') kind_string_assoc)

  let string_of_numeric_option =
    fun opt -> List.assoc opt numeric_opt_string_assoc

  let numeric_option_of_string = fun s ->
    fst (List.find (fun (_, s') -> s = s') numeric_opt_string_assoc)

  let class_of_kind = function
    | SQL_TinyInt | SQL_MediumInt | SQL_Int | SQL_BigInt -> SQL_C_Int
    | SQL_Double | SQL_Float | SQL_Decimal               -> SQL_C_Real
    | _                                                  -> SQL_C_Char

  let kind_uses_display_width = fun kind ->
    match class_of_kind kind with
      | SQL_C_Int | SQL_C_Real -> Dbf_misc.Maybe
      | SQL_C_Char ->
          match kind with
            | SQL_Char | SQL_VarChar -> Dbf_misc.Yes
            | _                      -> Dbf_misc.No

  let kind_uses_precision = fun kind ->
    match class_of_kind kind with
      | SQL_C_Real -> Dbf_misc.Maybe
      | _          -> Dbf_misc.No

  let options_of_kind = fun kind ->
    match class_of_kind kind with
      | SQL_C_Int | SQL_C_Real -> snd (List.split numeric_opt_string_assoc)
      | _                      -> []

  let get_display_size = function
    | SQL_db.TinyInt   (iopt, _)
    | SQL_db.MediumInt (iopt, _)
    | SQL_db.Int       (iopt, _)
    | SQL_db.BigInt    (iopt, _)
        -> iopt

    | SQL_db.Double  (opt, _)
    | SQL_db.Float   (opt, _)
    | SQL_db.Decimal (opt, _)
        -> Dbf_misc.apply_opt fst opt

    | SQL_db.Char i
    | SQL_db.VarChar i
      -> Some i

    | _ -> None

  let get_precision = function
    | SQL_db.Double  (opt, _)
    | SQL_db.Float   (opt, _)
    | SQL_db.Decimal (opt, _)
        -> Dbf_misc.apply_opt snd opt

    | _ -> None

  let get_options_as_string = function
    | SQL_db.Double  (_, opt)
    | SQL_db.Float   (_, opt)
    | SQL_db.Decimal (_, opt)
        -> Some (string_of_numeric_option opt)

    | _ -> None

  let type_of_string = fun ?dispsize ?precision ?(options = "") name ->
    let kind =
      try  kind_of_string name
      with Not_found -> raise (Invalid_type ("Unknown type: " ^ name))
    in
      begin
        match (kind_uses_display_width kind, dispsize) with
          | (No, Some _) -> raise (Invalid_type "Type doesn't support display size option")
          | (Yes, None)  -> raise (Invalid_type "Type requires display size option")
          | _ -> ()
      end; begin
        match (kind_uses_precision kind, precision) with
          | (No, Some _) -> raise (Invalid_type "Type doesn't support precision option")
          | (Yes, None)  -> raise (Invalid_type "Type requires precision option");
          | _ -> ()
      end;
      let options_list = options_of_kind kind in
        if options <> "" && not (List.mem options options_list) then begin
          let msg = Printf.sprintf "Unknown options %s for type %s" options name in
            raise (Invalid_type msg)
        end;
        let dispsize_and_precision_or_nothing = fun () ->
          match (dispsize, precision) with
            | (None, None)       -> None
            | (Some i1, Some i2) -> Some (i1, i2)
            | _ ->
                raise
                  (Invalid_type
                     "Need to give display size AND precision OR nothing at all")
        in
        let int_type = fun f_ty ->
          f_ty dispsize (numeric_option_of_string options)
        and real_type = fun f_ty ->
          let opt1 = dispsize_and_precision_or_nothing ()
          and opt2 = numeric_option_of_string options in
            f_ty opt1 opt2
        in
          match name with
            | "TINYINT"   -> int_type (fun ds opt -> SQL_db.TinyInt   (ds, opt))
            | "MEDIUMINT" -> int_type (fun ds opt -> SQL_db.MediumInt (ds, opt))
            | "INT"       -> int_type (fun ds opt -> SQL_db.Int       (ds, opt))
            | "BIGINT"    -> int_type (fun ds opt -> SQL_db.BigInt    (ds, opt))

            | "DOUBLE"  -> real_type (fun iopt opt -> SQL_db.Double  (iopt, opt))
            | "FLOAT"   -> real_type (fun iopt opt -> SQL_db.Float   (iopt, opt))
            | "DECIMAL" -> real_type (fun iopt opt -> SQL_db.Decimal (iopt, opt))

            | "CHAR"    -> SQL_db.Char    (Dbf_misc.unopt dispsize)
            | "VARCHAR" -> SQL_db.VarChar (Dbf_misc.unopt dispsize)

            | "TINYBLOB"   -> SQL_db.TinyBlob
            | "BLOB"       -> SQL_db.Blob
            | "MEDIUMBLOB" -> SQL_db.MediumBlob
            | "LONGBLOB"   -> SQL_db.LongBlob
            | "TINYTEXT"   -> SQL_db.TinyText
            | "TEXT"       -> SQL_db.Text
            | "MEDIUMTEXT" -> SQL_db.MediumText
            | "LONGTEXT"   -> SQL_db.LongText

            | _ -> raise (Invalid_type name)

  let string_of_type = fun ty -> string_of_kind (kind_of_type ty)

  let string_of_type_options = fun ty ->
    match ty with
      | SQL_db.TinyInt   (_, opt)
      | SQL_db.MediumInt (_, opt)
      | SQL_db.Int       (_, opt)
      | SQL_db.BigInt    (_, opt)
        -> Some (string_of_numeric_option opt)

      | SQL_db.Double  (_, opt)
      | SQL_db.Float   (_, opt)
      | SQL_db.Decimal (_, opt)
        -> Some (string_of_numeric_option opt)

      | _ -> None

  let fullstring_of_type = fun ty ->
    let opt_of_string = fun s ->
      if s = "" then None else Some s
    and intopt_string = fun iopt ->
      Dbf_misc.apply_opt (Printf.sprintf "(%d)") iopt
    and int2opt_string = fun i2opt ->
      Dbf_misc.apply_opt
        (fun (i1, i2) -> Printf.sprintf "(%d, %d)" i1 i2) i2opt
    in
      match ty with
        | SQL_db.TinyInt   (i, opt)
        | SQL_db.MediumInt (i, opt)
        | SQL_db.Int       (i, opt)
        | SQL_db.BigInt    (i, opt)
          -> join_opt
            ([Some (string_of_type ty);
              intopt_string (i);
              opt_of_string (string_of_numeric_option opt)])

        | SQL_db.Double  (i, opt)
        | SQL_db.Float   (i, opt)
        | SQL_db.Decimal (i, opt)
          -> join_opt
            ([Some (string_of_type ty);
              int2opt_string (i);
              opt_of_string (string_of_numeric_option opt)])

        | SQL_db.Char    i
        | SQL_db.VarChar i
          -> join_opt
            ([Some (string_of_type ty);
              Some (Printf.sprintf "(%d)" i)])

        | SQL_db.TinyBlob   -> string_of_type ty
        | SQL_db.Blob       -> string_of_type ty
        | SQL_db.MediumBlob -> string_of_type ty
        | SQL_db.LongBlob   -> string_of_type ty

        | SQL_db.TinyText   -> string_of_type ty
        | SQL_db.Text       -> string_of_type ty
        | SQL_db.MediumText -> string_of_type ty
        | SQL_db.LongText   -> string_of_type ty

end
