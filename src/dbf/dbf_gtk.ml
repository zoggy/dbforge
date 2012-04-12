(*********************************************************************************)
(*                DBForge                                                        *)
(*                                                                               *)
(*    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

open Dbf_gtk2Misc
open Dbf_sql
open Dbf_misc
open Dbf_dbStores

module TGUI  = Dbf_tableGUI
module CGUI  = Dbf_columnGUI
module VTGUI = Dbf_virtualTableGUI
module IGUI  = Dbf_indexGUI
module QGUI  = Dbf_queryGUI

type compoment_view = {
  cv_view   : GTree.view;
  cv_type_c : GTree.view_column;
  cv_name_c : GTree.view_column;
}

type component =
  | C_Table  of SQL_db.table
  | C_Column of SQL_db.column
  | C_VTable of SQL_db.vtable
  | C_Index  of SQL_db.index

class components_model_store () =
  let columns    = new GTree.column_list in
  let c_type     = columns#add Gobject.Data.string in
  let c_name     = columns#add Gobject.Data.string in
  let c_caml     = columns#add Gobject.Data.caml in
  let list_store = GTree.list_store columns in
object (self)
  inherit GTree.list_store
    (Gobject.try_cast list_store#as_model "GtkListStore")

  method set_data = fun data ~row ->
    self#set ~row ~column:c_caml data;
    self#update_display row

  method get_data = fun ~row ->
    self#get ~row ~column:c_caml

  method append_data = fun data ->
    self#set_data data ~row:(self#append ())

  method prepend_data = fun data ->
    self#set_data data ~row:(self#prepend ())

  method insert_data = fun i data ->
    self#set_data data ~row:(self#insert i)

  method update_display = fun iter ->
    match self#get_data ~row:iter with
      | C_Table table ->
          self#set ~row:iter ~column:c_type "Table";
          self#set ~row:iter ~column:c_name (table.SQL_db.ta_name)
      | C_Column column ->
          self#set ~row:iter ~column:c_type "Column";
          self#set ~row:iter ~column:c_name (column.SQL_db.col_name)
      | C_VTable vtable ->
          self#set ~row:iter ~column:c_type "Virtual table";
          self#set ~row:iter ~column:c_name (vtable.SQL_db.vt_name)
      | C_Index index ->
          self#set ~row:iter ~column:c_type "Index";
          self#set ~row:iter ~column:c_name (index.SQL_db.idx_name)

  method create_view = fun ?(view = GTree.view ()) () ->
    let type_column =
      GTree.view_column ~title:"Type"
	~renderer:(GTree.cell_renderer_text [], [("text", c_type)])
	()
    in
    let name_column =
      GTree.view_column ~title:"Name"
	~renderer:(GTree.cell_renderer_text [], [("text", c_name)])
	()
    in
      i_int (view#append_column name_column);
      view#set_model (Some self#coerce);
      { cv_view = view; cv_type_c = type_column; cv_name_c = name_column; }

  initializer
    self#set_sort_column_id 0 `ASCENDING;
end

class ['a] remove_dialog_box () =
  let model  = new components_model_store () in
  let view   = model#create_view () in
  let dialog = GWindow.dialog ~title:"Really remove ?"
                 ~position:`MOUSE ()
  in
object(self)
  inherit ['a] GWindow.dialog
    (Gobject.try_cast dialog#as_window "GtkDialog")

  method set_components = fun components ->
    view.cv_view#set_model (None);
    List.iter (fun data -> model#prepend_data data) components;
    view.cv_view#set_model (Some model#coerce)

  initializer
    self#add_button_stock `YES `YES;
    self#add_button_stock `NO  `NO;
    let _ = GMisc.label
              ~text:"This will remove the following compoments. Continue ?"
              ~line_wrap:true
              ~packing:(self#vbox#pack ~expand:false)
              ()
    in
    let scrolled_window =
      GBin.scrolled_window
        ~border_width:5
        ~shadow_type:`IN
        ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
        ~packing:(self#vbox#pack ~expand:true)
        ()
    in
      scrolled_window#add view.cv_view#coerce;
      view.cv_view#selection#set_select_function (fun _ _ -> false)
end

(* Main GUI *)
class gui () =
  let glade = new Dbf_gladeWidgets.main_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
  let table_model  = new table_store ()
  and column_model = new column_store ()
  and vtable_model = new vtable_store ()
  and index_model  = new index_store ()
  and query_model  = new query_store () in
  let table_view   = table_model#create_view ~view:glade#tables_view ()
  and column_view  = column_model#create_view ~view:glade#columns_view ()
  and vtable_view  = vtable_model#create_view ~view:glade#vtables_view ()
  and index_view   = index_model#create_view ~view:glade#indexes_view ()
  and query_view   = query_model#create_view ~view:glade#queries_view ()
  in
object (self)
  val mutable db = SQL_db.create_empty ()
  val mutable current_column_model = column_model
  val mutable saved     = true
  val mutable filename  = None

  method vbox = glade#box0
  method reparent = glade#reparent

  method private _do_quit =
    let button =
      GToolbox.question_box ~title:"Quit ?"
        ~buttons:["Yes"; "No"] ~default:2 "Are you sure you want to quit ?"
    in
      button = 1

  method private _check_saved =
    if not saved then begin
      let answer = GToolbox.question_box ~title:"DB not saved"
                     ~buttons:["Yes"; "No"]
                     "You current database is not saved. Continue ?"
      in
        answer = 1
    end else
      true

  (* Setters/Getters *)
  method private _set_column_model = fun (model : #column_store) ->
    current_column_model <- model;
    column_view.col_view#set_model (Some model#coerce)

  method private _get_selection =
    let table_selection =
      match table_view.tv_view#selection#get_selected_rows with
        | [path] ->
            let iter = table_model#get_iter path in
              Some (path, table_model#get_data ~row:iter)
        | _  -> None
    in
      match table_selection with
        | None -> None
        | Some ((_, _) as ts) -> begin
            match column_view.col_view#selection#get_selected_rows with
              | [path] ->
                  let iter = current_column_model#get_iter path in
                  let r    = current_column_model#get_data ~row:iter in
                    Some (ts, Some (path, r))
              | _ -> Some (ts, None)
          end

   method private _update_display =
     table_view.tv_view#set_model  None;
     vtable_view.vt_view#set_model None;
     index_view.idx_view#set_model None;
     query_view.qry_view#set_model None;
     table_model#clear ();
     vtable_model#clear ();
     index_model#clear ();
     query_model#clear ();
     List.iter
       (fun t ->
          (let store = new column_store () in
           let data  = { td_table = t; td_model = store; } in
          List.iter (fun c -> store#append_data c) t.SQL_db.ta_columns;
             table_model#append_data data))
       db.SQL_db.db_tables;
     List.iter (fun v -> vtable_model#append_data v) db.SQL_db.db_vtables;
     List.iter (fun i -> index_model#append_data i) db.SQL_db.db_indexes;
     List.iter (fun i -> query_model#append_data i) db.SQL_db.db_queries;
     table_view.tv_view#set_model (Some table_model#coerce);
     vtable_view.vt_view#set_model (Some vtable_model#coerce);
     index_view.idx_view#set_model (Some index_model#coerce);
     query_view.qry_view#set_model (Some query_model#coerce)


  method open_file name =
    begin
      try
        db <- Dbf_sql_io.db_of_file name;
      with
      | Dbf_sql_io.Invalid_db_file ->
          GToolbox.message_box ~title:"Load error"
            "Invalid database file"
    end;
    self#_update_display;
    filename <- Some name;
    saved <- true

 (* Callbacks *)
  method private _cb__quit = fun () ->
    if self#_do_quit then GMain.quit ()

  method private _cb__new = fun () ->
    if self#_check_saved then
      self#clear ()

  method private _cb__open = fun () ->
    if self#_check_saved then begin
      let dialog = GWindow.file_chooser_dialog ~action:`OPEN
                     ~title:"Open database"
                     ~position:`MOUSE () in
        dialog#add_select_button_stock `OPEN `OK;
        match dialog#run () with
          | `OK ->
              let name = Dbf_misc.unopt dialog#filename in
	      dialog#destroy ();
              self#open_file name
          | `DELETE_EVENT -> dialog#destroy ()
    end

  method private _cb__save = fun () ->
    match filename with
      | None      -> self#_cb__save_as ()
      | Some name -> self#save name

  method private _cb__save_as = fun () ->
    let dialog = GWindow.file_chooser_dialog ~action:`SAVE
                   ~title:"Save database"
                   ~position:`MOUSE () in
      dialog#add_select_button_stock `SAVE `OK;
      match dialog#run () with
        | `OK ->
            self#save (Dbf_misc.unopt dialog#filename);
            dialog#destroy ()
        | `DELETE_EVENT -> dialog#destroy ()

  method private _cb__table_selection_changed = fun () ->
    match self#_get_selection with
      | Some ((_, { td_model = model; td_table = table }), _) ->
	  self#_set_column_model model
      | _ ->
          self#_set_column_model column_model

  method private _document_changed = fun () ->
    saved <- false

  method private _cb__add_table = fun () ->
    let table_window =
      new TGUI.table_gui self#_table_gui_answer
        { TGUI.tgi_name    = SQL_db.create_table_name db ();
          TGUI.tgi_comment = "";
          TGUI.tgi_logged  = false ;
          TGUI.tgi_pkey    = [];
          TGUI.tgi_columns = None; }
    in
      table_window#start ()

  method private _cb__add_vtable = fun () ->
    let defaults = { VTGUI.vtgi_name   = SQL_db.create_vtable_name db ();
                     VTGUI.vtgi_vtable = None; } in
    let vtable_window =
      new VTGUI.vtable_gui (db.SQL_db.db_tables)
        self#_vtable_gui_answer defaults
    in
      vtable_window#start ()

  method private _cb__add_column = fun () ->
    match self#_get_selection with
      | None -> ()
      | Some ((_, { td_table = table }), _) ->
          let name = SQL_db.create_column_name table () in
          let defaults = { CGUI.cgi_name       = name;
                           CGUI.cgi_comment    = "";
                           CGUI.cgi_type       = SQL_db.Int (None, SQL_db.NO_None);
                           CGUI.cgi_nullable   = true;
                           CGUI.cgi_options    = Dbf_misc.StringMap.empty;
                           CGUI.cgi_spec_ty    = Dbf_misc.StringMap.empty;
                           CGUI.cgi_ocaml_type = "";
                           CGUI.cgi_sql2ml     = "";
                           CGUI.cgi_ml2sql     = ""; }
          in
          let column_window = new CGUI.column_gui self#_column_gui_answer defaults in
            column_window#start ()

  method private _cb__add_index = fun () ->
    match self#_get_selection with
      | None -> ()
      | Some ((_, { td_table = table }), _) ->
          let defaults = { IGUI.idxgi_name    = SQL_db.create_index_name db ();
                           IGUI.idxgi_table   = table;
                           IGUI.idxgi_columns = [];
                           IGUI.idxgi_unique  = false; }
          in
          let index_window = new IGUI.index_gui self#_index_gui_answer defaults in
            index_window#start ()

  method private _check_gui_query q =
    let q = { SQL_db.qry_name = q.QGUI.qrygi_name ;
	      SQL_db.qry_db   = db ;
	      SQL_db.qry_comment = "" ;
	      SQL_db.qry_query = q.QGUI.qrygi_query ;
	    }
    in
    match SQL_db.query_state q with
      SQL_db.Query_ok _ -> ()
    | state ->
	let s = SQL_db.string_of_query_state state in
	raise (Invalid_input s)

  method private _cb__add_query = fun () ->
    let defaults = { QGUI.qrygi_name    = SQL_db.create_query_name db ();
		     QGUI.qrygi_query   = "" ;
                     QGUI.qrygi_comment = "";
		   }
    in
    let query_window = new QGUI.query_gui
	self#_query_gui_answer
	self#_check_gui_query
	defaults
    in
    query_window#start ()

  method private _cb__remove_table = fun () ->
    match table_view.tv_view#selection#get_selected_rows with
      | [path] -> self#_remove_table_internal path
      | _      -> ()

  method private _cb__remove_vtable = fun () ->
    match vtable_view.vt_view#selection#get_selected_rows with
      | [path] ->
          let iter = vtable_model#get_iter path in
          let vtable = vtable_model#get_data ~row:iter in
            (* FIXME: use an internal function *)
            self#_document_changed ();
            i_bool (vtable_model#remove iter);
            SQL_db.unlink_vtable vtable
      | _ -> ()

  method private _cb__remove_column = fun () -> (* FIXME: use an internal function ? *)
    match self#_get_selection with
      | Some ((_, table), Some (path, column)) ->
          let vtables = SQL_db.vtables_using_column column
          and indexes = SQL_db.indexes_using_column column in
          let vtables = List.map (fun vt -> C_VTable vt) vtables
          and indexes = List.map (fun i  -> C_Index i) indexes
          in
          let do_remove =
            if vtables = [] && indexes = [] then
              true
            else
              let dialog = new remove_dialog_box () in
                dialog#set_components (vtables @ indexes);
                let r = dialog#run () in
                  dialog#destroy ();
                  r = `YES
          in
            if do_remove then begin
              self#_document_changed ();
              let (vtables, indexes, _) = SQL_db.unlink_column column
              and iter = current_column_model#get_iter path in
                i_bool (current_column_model#remove iter);
                List.iter
                  (fun vt ->
                     self#_remove_vtable_from_store vt.SQL_db.vt_name)
                  vtables;
                List.iter
                  (fun idx ->
                     self#_remove_index_from_store idx.SQL_db.idx_name)
                  indexes
            end
      | _ -> ()

  method private _cb__remove_index = fun () ->
    match index_view.idx_view#selection#get_selected_rows with
      | [path] ->
          (* FIXME: use an internal function *)
          let iter  = index_model#get_iter path in
          let index = index_model#get_data iter in
            self#_document_changed ();
            i_bool (index_model#remove iter);
            SQL_db.unlink_index index
      | _ -> ()

  method private _cb__remove_query = fun () ->
    match query_view.qry_view#selection#get_selected_rows with
      | [path] ->
          (* FIXME: use an internal function *)
          let iter  = query_model#get_iter path in
          let query = query_model#get_data iter in
          self#_document_changed ();
          i_bool (query_model#remove iter);
          SQL_db.unlink_query query
      | _ -> ()

  method private _cb__edit_table = fun () ->
    match table_view.tv_view#selection#get_selected_rows with
      | [path] -> let iter = table_model#get_iter path in
        let table_data = table_model#get_data ~row:iter in
	let table = table_data.td_table in
        let defaults = { TGUI.tgi_name    = table.SQL_db.ta_name;
                         TGUI.tgi_comment = table.SQL_db.ta_comment;
                         TGUI.tgi_logged  = table.SQL_db.ta_logged ;
                         TGUI.tgi_pkey    = table.SQL_db.ta_pkey;
                         TGUI.tgi_columns = Some table.SQL_db.ta_columns; }
        in
        let table_gui =
          new TGUI.table_gui
            (self#_table_gui_update_answer
               (table_model#get_row_reference path) table) defaults
        in
          table_gui#start ()
      | _      -> ()

  method private _cb__edit_column = fun () ->
    match self#_get_selection with
      | Some (_, Some (path, column)) ->
          let defaults = { CGUI.cgi_name       = column.SQL_db.col_name;
                           CGUI.cgi_comment    = column.SQL_db.col_comment;
                           CGUI.cgi_type       = column.SQL_db.col_type;
                           CGUI.cgi_nullable   = column.SQL_db.col_nullable;
                           CGUI.cgi_options    = column.SQL_db.col_spec_options;
                           CGUI.cgi_spec_ty    = column.SQL_db.col_spec_ty;
                           CGUI.cgi_ocaml_type = column.SQL_db.col_ocaml_ty;
                           CGUI.cgi_sql2ml     = column.SQL_db.col_sql2ml;
                           CGUI.cgi_ml2sql     = column.SQL_db.col_ml2sql; }
          in
          let column_gui =
            new CGUI.column_gui
              (self#_column_gui_update_answer
                 (current_column_model#get_row_reference path) column)
              defaults
          in
            column_gui#start ()
      | _ -> ()

  method private _cb__edit_vtable = fun () ->
    match vtable_view.vt_view#selection#get_selected_rows with
      | [path] ->
          let row    = vtable_model#get_row_reference path in
          let vtable = vtable_model#get_data ~row:row#iter in
          let vtable_window =
            new VTGUI.vtable_gui (db.SQL_db.db_tables)
              (self#_vtable_gui_update_answer row vtable)
              { VTGUI.vtgi_name   = vtable.SQL_db.vt_name;
                VTGUI.vtgi_vtable = Some vtable; }
          in
            vtable_window#start ()
      | _ -> ()

  method private _cb__edit_index = fun () ->
    match index_view.idx_view#selection#get_selected_rows with
      | [path] ->
          let row      = index_model#get_row_reference path in
          let index    = index_model#get_data ~row:row#iter in
          let index_window =
            new IGUI.index_gui
              (self#_index_gui_update_answer row index)
              { IGUI.idxgi_name    = index.SQL_db.idx_name;
                IGUI.idxgi_table   = SQL_db.table_of_index index;
                IGUI.idxgi_columns = index.SQL_db.idx_columns;
                IGUI.idxgi_unique  = index.SQL_db.idx_unique; }
          in
            index_window#start ()
      | _ -> ()

  method private _cb__edit_query = fun () ->
    match query_view.qry_view#selection#get_selected_rows with
      | [path] ->
          let row      = query_model#get_row_reference path in
          let query    = query_model#get_data ~row:row#iter in
          let query_window =
            new QGUI.query_gui
              (self#_query_gui_update_answer row query)
	      self#_check_gui_query
              { QGUI.qrygi_name    = query.SQL_db.qry_name;
                QGUI.qrygi_query = query.SQL_db.qry_query;
                QGUI.qrygi_comment  = query.SQL_db.qry_comment; }
          in
          query_window#start ()
      | _ -> ()

  (* Answers from sub-windows *)
  method private _table_gui_answer = fun input ->
    if not (SQL_db.validate_name input.TGUI.tgi_name) then
      raise (Invalid_input "Invalid table name");
    if SQL_db.table_by_name_opt db input.TGUI.tgi_name <> None then
      raise (Invalid_input "Name already used");
    self#_add_table_internal
      ~name:input.TGUI.tgi_name
      ~comment:input.TGUI.tgi_comment
      ~logged:input.TGUI.tgi_logged

  method private _table_gui_update_answer = fun row_ref table input ->
    let new_name = ref None in
      if table.SQL_db.ta_name <> input.TGUI.tgi_name then begin
        if not (SQL_db.validate_name input.TGUI.tgi_name) then
          raise (Invalid_input "Invalid table name");
        if SQL_db.table_by_name_opt db input.TGUI.tgi_name <> None then
          raise (Invalid_input "Name already used");
        new_name := Some input.TGUI.tgi_name
      end;
      self#_update_table_internal row_ref table
        ?name:!new_name ~comment:input.TGUI.tgi_comment
        ~logged:input.TGUI.tgi_logged
        ~pkey:input.TGUI.tgi_pkey ();

  method private _column_gui_answer = fun input ->
    match self#_get_selection with
      | None -> Dbf_misc.ie ()
      | Some ((_, table_data), _) ->
	  let table = table_data.td_table
	  and model = table_data.td_model in
            if not (SQL_db.validate_name input.CGUI.cgi_name) then
              raise (Invalid_input "Invalid column name");
            if SQL_db.column_by_name_opt table input.CGUI.cgi_name <> None then
	      raise (Invalid_input "Name already used");
            self#_add_column_internal
	      ~table:      table
	      ~model:      model
	      ~name:       input.CGUI.cgi_name
	      ~comment:    input.CGUI.cgi_comment
	      ~ty:         input.CGUI.cgi_type
	      ~nullable:   input.CGUI.cgi_nullable
              ~options:    input.CGUI.cgi_options
              ~spec_ty:    input.CGUI.cgi_spec_ty
	      ~ocaml_type: input.CGUI.cgi_ocaml_type
	      ~sql2ml:     input.CGUI.cgi_sql2ml
	      ~ml2sql:     input.CGUI.cgi_ml2sql
	      ()

  (*  FIXME: catch exceptions instead of checking input *)
  method private _column_gui_update_answer = fun row_ref column input ->
    let new_name = ref None in
      if column.SQL_db.col_name <> input.CGUI.cgi_name then begin
        if not (SQL_db.validate_name input.CGUI.cgi_name) then
          raise (Invalid_input "Invalid column name");
        let found_column =
          SQL_db.column_by_name_opt
            column.SQL_db.col_table
            input.CGUI.cgi_name
        in
          if found_column <> None then
            raise (Invalid_input "Name already used");
          new_name := Some input.CGUI.cgi_name
      end;
      self#_update_column_internal row_ref column
        ?name:       !new_name
        ~comment:    input.CGUI.cgi_comment
	~ty:         input.CGUI.cgi_type
	~nullable:   input.CGUI.cgi_nullable
        ~options:    input.CGUI.cgi_options
        ~spec_ty:    input.CGUI.cgi_spec_ty
	~ocaml_type: input.CGUI.cgi_ocaml_type
	~sql2ml:     input.CGUI.cgi_sql2ml
	~ml2sql:     input.CGUI.cgi_ml2sql
        ()

  method private _vtable_gui_answer = fun input ->
    try
      SQL_db.link_vtable_to_db (Dbf_misc.unopt input.VTGUI.vtgi_vtable);
      vtable_model#append_data (Dbf_misc.unopt input.VTGUI.vtgi_vtable)
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already used")

  method private _vtable_gui_update_answer = fun row_ref vtable input ->
    try
      let new_vtable = Dbf_misc.unopt input.VTGUI.vtgi_vtable in
        if vtable.SQL_db.vt_name <> new_vtable.SQL_db.vt_name then
          SQL_db.rename_vtable vtable new_vtable.SQL_db.vt_name;
        (* FIXME: change SQL interface *)
        vtable.SQL_db.vt_ftable <- new_vtable.SQL_db.vt_ftable;
        vtable.SQL_db.vt_join   <- new_vtable.SQL_db.vt_join;
        vtable_model#update_display row_ref#iter
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already used")

  method private _index_gui_answer = fun input ->
    try
      let index = SQL_db.insert_index
                    ~name:input.IGUI.idxgi_name
                    ~columns:input.IGUI.idxgi_columns
                    ~unique:input.IGUI.idxgi_unique
      in
        (* FIXME: use an another method for this *)
        index_model#append_data index
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already use")
      | SQL_db.Invalid_name name ->
          raise (Invalid_input "Invalid name");
      | SQL_db.Invalid_args s ->
          raise (Invalid_input s)

  method private _query_gui_answer = fun input ->
    try
      let query = SQL_db.insert_query db
          ~name:input.QGUI.qrygi_name
          ~comment:input.QGUI.qrygi_comment
          ~query:input.QGUI.qrygi_query
      in
      (* FIXME: use an another method for this *)
      query_model#append_data query
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already use")
      | SQL_db.Invalid_name name ->
          raise (Invalid_input "Invalid name");
      | SQL_db.Invalid_args s ->
          raise (Invalid_input s)

  method private _index_gui_update_answer = fun row index input ->
    try
      SQL_db.update_index index
        input.IGUI.idxgi_name
        input.IGUI.idxgi_columns
        input.IGUI.idxgi_unique;
      index_model#update_display row#iter
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already use")
      | SQL_db.Invalid_name name ->
          raise (Invalid_input "Invalid name");
      | SQL_db.Invalid_args s ->
          raise (Invalid_input s)

  method private _query_gui_update_answer = fun row query input ->
    try
      SQL_db.update_query query
        input.QGUI.qrygi_name
        input.QGUI.qrygi_query
        input.QGUI.qrygi_comment;
      query_model#update_display row#iter
    with
      | SQL_db.Duplicated_name name ->
          raise (Invalid_input "Name already use")
      | SQL_db.Invalid_name name ->
          raise (Invalid_input "Invalid name");
      | SQL_db.Invalid_args s ->
          raise (Invalid_input s)

  (* DB manipulation *)
  method private _add_table_internal = fun ~name ~comment ~logged ->
    let table = SQL_db.insert_table db ~name ~comment ~logged in
    let model = new column_store () in
    let table_data = { td_table = table; td_model = model; } in
    let _ = table_model#append_data table_data in
    self#_document_changed ()

  method private _add_column_internal =
    fun ~table ~model ~name ~comment ~ty ?(nullable = true)
        ?(ocaml_type = "") ?(sql2ml = "") ?(ml2sql = "")
        ?(options = Dbf_misc.StringMap.empty)
        ?(spec_ty = Dbf_misc.StringMap.empty)
        () ->

      let column = SQL_db.insert_column table ~name ~comment ~ty ~nullable () in
        column.SQL_db.col_spec_options <- options;
        column.SQL_db.col_spec_ty      <- spec_ty;
	column.SQL_db.col_ocaml_ty     <- ocaml_type;
	column.SQL_db.col_sql2ml       <- sql2ml;
	column.SQL_db.col_ml2sql       <- ml2sql;
	model#append_data column;
        self#_document_changed ()

  method private _remove_table_internal = fun path ->
    let iter = table_model#get_iter path in
    let table_data = table_model#get_data ~row:iter in
    let vtables = SQL_db.vtables_using_table table_data.td_table
    and indexes = SQL_db.indexes_using_table table_data.td_table in
    let vtables = List.map (fun vt -> C_VTable vt) vtables
    and indexes = List.map (fun i  -> C_Index i) indexes
    in
    let do_remove =
      if vtables = [] && indexes = [] then
        true
      else
        let dialog = new remove_dialog_box () in
          dialog#set_components (vtables @ indexes);
          let r = dialog#run () in
            dialog#destroy ();
            r = `YES
    in
      if do_remove then begin
        self#_document_changed ();
        (* FIXME: Unref model ??? *)
        let (vtables, indexes) = SQL_db.unlink_table table_data.td_table in
          i_bool (table_model#remove iter);
          List.iter
            (fun vt ->
               self#_remove_vtable_from_store vt.SQL_db.vt_name)
            vtables;
          List.iter
            (fun idx ->
               self#_remove_index_from_store idx.SQL_db.idx_name)
            indexes
      end

  (* XXX Is table can be partially updated ? *)
  method private _update_table_internal =
    fun row_ref table ?name ~comment ~logged ~pkey () ->
      begin
	match name with
          | None      -> ()
          | Some name ->
              SQL_db.rename_table table name
      end;
      SQL_db.set_primary_key table pkey;
      table.SQL_db.ta_comment <- comment;
      table.SQL_db.ta_logged <- logged;
      table_model#update_display row_ref#iter;
      self#_document_changed ()

  method private _update_column_internal =
    fun row_ref column
        ~name ~comment ~ty ~nullable ~options ~spec_ty
        ~ocaml_type ~sql2ml ~ml2sql
        () ->
      begin
        match name with
          | None      -> ()
          | Some name ->
              SQL_db.rename_column column name
      end;
      column.SQL_db.col_type         <- ty;
      column.SQL_db.col_comment      <- comment;
      column.SQL_db.col_nullable     <- nullable;
      column.SQL_db.col_spec_options <- options;
      column.SQL_db.col_spec_ty      <- spec_ty;
      column.SQL_db.col_ocaml_ty     <- ocaml_type;
      column.SQL_db.col_sql2ml       <- sql2ml;
      column.SQL_db.col_ml2sql       <- ml2sql;
      current_column_model#update_display row_ref#iter;
      self#_document_changed ()

  method private _remove_vtable_from_store = fun name ->
    let rref = ref None in
      vtable_model#foreach
        (fun path iter ->
           let vt = vtable_model#get_data ~row:iter in
             if vt.SQL_db.vt_name = name then begin
               rref := Some (vtable_model#get_row_reference path);
               true
             end else
               false);
      match !rref with
        | None      -> ()
        | Some rref ->
            let _ = vtable_model#get_data ~row:rref#iter in
            (* Virtual table in already unlinked from database *)
            self#_document_changed ();
            i_bool (vtable_model#remove rref#iter)

  method private _remove_index_from_store = fun name ->
    let rref = ref None in
      index_model#foreach
        (fun path iter ->
           let idx = index_model#get_data ~row:iter in
             if idx.SQL_db.idx_name = name then begin
               rref := Some (index_model#get_row_reference path);
               true
         end else
           false);
      match !rref with
        | None      -> ()
        | Some rref ->
            let _idx = index_model#get_data ~row:rref#iter in
            (* Index is already unlinked from database *)
            self#_document_changed ();
            i_bool (index_model#remove rref#iter)

  (* I/O *)
  method save = fun name ->
    let chan = open_out name in
      output_string chan (Xml.to_string_fmt (Dbf_sql_io.xml_of_db db));
      close_out chan;
      filename <- Some name; saved <- true

  method save_current = self#_cb__save ()

  method clear = fun () ->
    db <- SQL_db.create_empty ();
    self#_update_display;
    filename <- None; saved <- true;
    self#_update_display

  initializer
  let handlers =
    [("on_new_menuitem_activate",           `Simple self#_cb__new);
     ("on_open_menuitem_activate",          `Simple self#_cb__open);
     ("on_save_menuitem_activate",          `Simple self#_cb__save);
     ("on_saveas_menuitem_activate",        `Simple self#_cb__save_as);
     ("on_quit_menuitem_activate",          `Simple self#_cb__quit);
     ("on_add_table_menuitem_activate",     `Simple self#_cb__add_table);
     ("on_add_vtable_menuitem_activate",    `Simple self#_cb__add_vtable);
     ("on_remove_table_menuitem_activate",  `Simple self#_cb__remove_table);
     ("on_remove_vtable_menuitem_activate", `Simple self#_cb__remove_vtable);
     ("on_edit_table_menuitem_activate",    `Simple self#_cb__edit_table);
     ("on_edit_vtable_menuitem_activate",   `Simple self#_cb__edit_vtable);
     ("on_add_column_menuitem_activate",    `Simple self#_cb__add_column);
     ("on_remove_column_menuitem_activate", `Simple self#_cb__remove_column);
     ("on_edit_column_menuitem_activate",   `Simple self#_cb__edit_column);
     ("on_add_index_menuitem_activate",     `Simple self#_cb__add_index);
     ("on_remove_index_menuitem_activate",  `Simple self#_cb__remove_index);
     ("on_edit_index_menuitem_activate",    `Simple self#_cb__edit_index);
     ("on_add_query_menuitem_activate",     `Simple self#_cb__add_query);
     ("on_remove_query_menuitem_activate",  `Simple self#_cb__remove_query);
     ("on_edit_query_menuitem_activate",    `Simple self#_cb__edit_query);
    ]
  in
    (* Finalize GUI *)
    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
    i (table_view.tv_view#selection#connect#changed
         self#_cb__table_selection_changed);
    (* Signals for double-clicks on view *)
    i (table_view.tv_view#connect#row_activated
         (fun _ _ -> self#_cb__edit_table ()));
    i (column_view.col_view#connect#row_activated
         (fun _ _ -> self#_cb__edit_column ()));
    i (vtable_view.vt_view#connect#row_activated
         (fun _ _ -> self#_cb__edit_vtable ()));
    i (index_view.idx_view#connect#row_activated
         (fun _ _ -> self#_cb__edit_index ()));
    i (query_view.qry_view#connect#row_activated
         (fun _ _ -> self#_cb__edit_query ()));
    (* FIXME: Ask for permission *)
    i (glade#toplevel#connect#destroy GMain.quit);
    glade#toplevel#show ()

end
