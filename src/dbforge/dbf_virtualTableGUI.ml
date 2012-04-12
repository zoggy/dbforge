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

open Dbf_gtk2Misc
open Dbf_sql
open Dbf_misc

module SW = Dbf_selectWidget
module DW = Dbf_dbWidget

type table_view = {
  tv_view  : GTree.view;
  tv_model : GTree.list_store;
  tv_nc    : string GTree.column;
  tv_caml  : SQL_db.table GTree.column;
}

class vtable_constraints_gui cb (tables1, tables2) =
object (self)
  val glade = new Dbf_gladeWidgets.constraint_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false ()

  val view1 = new DW.db_widget `SINGLE
  val view2 = new DW.db_widget `SINGLE

  method start = fun () ->
    glade#toplevel#set_modal true;
    glade#toplevel#show ()

  method private _grab_input =
    match (view1#selected_columns, view2#selected_columns) with
      | ([c1], [c2]) -> (c1, c2)
      | _ ->
	  raise (Invalid_input "Please, select columns for constraint")

  method private _cb__cancel_button_clicked = fun () ->
    glade#toplevel#destroy ()

  method private _cb__ok_button_clicked = fun () ->
    try
      cb self#_grab_input;
      glade#toplevel#destroy ()
    with
      | Invalid_input msg ->
          GToolbox.message_box ~title:"Invalid input" msg

  initializer
  let handlers =
    [("on_cancel_button_clicked", `Simple self#_cb__cancel_button_clicked);
     ("on_ok_button_clicked",     `Simple self#_cb__ok_button_clicked)]
  in
    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
    view1#set_tables tables1;
    view2#set_tables tables2;
    let scroll1 = glade#view1_scrolled
    and scroll2 = glade#view2_scrolled
    in
      scroll1#add view1#view.DW.sv_view#coerce;
      scroll2#add view2#view.DW.sv_view#coerce
end

type join_constraint = {
  jc_column1: SQL_db.column;
  jc_column2: SQL_db.column;
}

let jc_to_tuple = fun jc ->
  (jc.jc_column1, jc.jc_column2)

type view = {
  sv_view : GTree.view;
  sv_c1   : GTree.view_column;
  sv_c2   : GTree.view_column;
}

class constraints_list_store () =
  let columns = new GTree.column_list in
  let c1 = columns#add Gobject.Data.string in
  let c2 = columns#add Gobject.Data.string in
  let camlc = columns#add Gobject.Data.caml in
  let list_store = GTree.list_store columns
  in
object (self)
  inherit GTree.list_store
    (Gobject.try_cast list_store#as_model "GtkListStore")

  method set_data = fun c ~row ->
    self#set ~row ~column:c1 (SQL_db.column_fullname c.jc_column1);
    self#set ~row ~column:c2 (SQL_db.column_fullname c.jc_column2);
    self#set ~row ~column:camlc c

  method get_data = fun ~row ->
    self#get ~row ~column:camlc

  method append_data = fun c ->
    self#set_data c (self#append ())

  method prepend_data = fun c ->
    self#set_data c (self#prepend ())

  method insert_data = fun i c ->
    self#set_data c (self#insert i)

  method clear_table_from_constraints = fun name ->
    let to_be_removed = ref [] in
      self#foreach
        (fun path iter ->
           let data = self#get_data ~row:iter in
           let t1   = data.jc_column1.SQL_db.col_table.SQL_db.ta_name
           and t2   = data.jc_column2.SQL_db.col_table.SQL_db.ta_name in
               if name = t1 || name = t2 then
                 to_be_removed := (self#get_row_reference path) :: !to_be_removed;
             false);
      List.iter
        (fun r -> i_bool (self#remove r#iter))
        !to_be_removed

  method move_constraints_to = fun name (store : constraints_list_store) ->
    let to_be_moved = ref [] in
      self#foreach
        (fun path iter ->
           let data = self#get_data ~row:iter in
           let t1   = data.jc_column1.SQL_db.col_table.SQL_db.ta_name
           and t2   = data.jc_column2.SQL_db.col_table.SQL_db.ta_name in
             if name = t1 || name = t2 then
               to_be_moved := (self#get_row_reference path) :: !to_be_moved;
             false);
      List.iter
        (fun r ->
           let iter  = r#iter in
           let data  = self#get_data ~row:iter in
           let data' = { jc_column1 = data.jc_column2;
                         jc_column2 = data.jc_column1; }
           in
             i_bool (self#remove iter);
             store#append_data data')
        !to_be_moved

  method create_view = fun ?(view = GTree.view ()) () ->
    let c1_column =
      GTree.view_column ~title:"First key"
	~renderer:(GTree.cell_renderer_text [], [("text", c1)])
	()
    and c2_column =
      GTree.view_column ~title:"Second key"
	~renderer:(GTree.cell_renderer_text [], [("text", c2)])
	()
    in
      c1_column#set_resizable true;
      c2_column#set_resizable true;
      i_int (view#append_column c1_column);
      i_int (view#append_column c2_column);
      view#set_model (Some self#coerce);
      { sv_view = view;
	sv_c1   = c1_column;
	sv_c2   = c2_column; }
end

type vtable_gui_input = {
  vtgi_name   : string;
  vtgi_vtable : SQL_db.vtable option;
}

class vtable_gui tables cb defaults =
  let glade = new Dbf_gladeWidgets.vtable_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
  let empty_constr_store = new constraints_list_store () in
  let constr_view = empty_constr_store#create_view ~view:glade#constr_view ()
  and select_w = new SW.select_and_order
                   ~label:"Join"
                   ~extract:(fun (t, _) -> t.SQL_db.ta_name)
                   ~title1:"Remaining tables"
                   ~title2:"Tables in join"
                   ()
  in
object (self)
  val mutable current_constr_store = empty_constr_store

  method private _set_constr_model = fun store ->
    constr_view.sv_view#set_model (Some store#coerce);
    current_constr_store <- store;
    (* FIXME: Check for a real signal *)
    self#_cb__constr_view_selection_changed ()

  method private _grab_input =
    match select_w#store2#get_iter_first with
      | None ->
          raise (Invalid_input "You must put at least one table in the join")
      | Some iter ->
          let fetch_columns = fun c_model ->
            match c_model#get_iter_first with
              | None -> []
              | Some iter -> let result = ref [] in
                  result := [jc_to_tuple (c_model#get_data ~row:iter)];
                  while c_model#iter_next iter do
                    result :=
                    (jc_to_tuple (c_model#get_data ~row:iter)) :: !result
                  done;
                  List.rev !result
          in
          let model  = select_w#store2 in
          let table  = fst (model#get_data ~row:iter) in
          let vtable = SQL_db.create_vtable glade#name_entry#text table in
            while model#iter_next iter do
              let (c_table, c_model) = model#get_data ~row:iter in
                SQL_db.do_join vtable c_table (fetch_columns c_model)
            done;
            { vtgi_name   = vtable.SQL_db.vt_name;
              vtgi_vtable = Some vtable; }

  method private _cb__constr_view_selection_changed = fun () ->
    glade#remove_constr_button#misc#set_sensitive
    (constr_view.sv_view#selection#get_selected_rows <> [])

  method private _constraint_gui_answer = fun model (c1, c2) ->
    model#append_data { jc_column1 = c1; jc_column2 = c2; }

  method private _cb__ok_button_clicked = fun () ->
    try
      cb self#_grab_input;
      glade#toplevel#destroy ()
    with
      | Invalid_input msg ->
          GToolbox.message_box ~title:"Invalid input" msg

  method private _cb__cancel_button_clicked = fun () ->
    glade#toplevel#destroy ()

  method private _cb__add_constraint = fun () ->
    let view  = select_w#view2.SW.st_view
    and model = select_w#store2 in

    let fetch_tables_until = fun iter ->
      let limit_path = model#get_path iter
      and result     = ref [] in
      let current    = Dbf_misc.unopt model#get_iter_first in
        while (model#get_path current) <> limit_path do
          result := (fst (model#get_data ~row:current)) :: !result;
          if not (model#iter_next current) then
            Dbf_misc.ie ()
        done;
        List.rev !result
    in
      match view#selection#get_selected_rows with
        | [path] ->
            if model#get_path (Dbf_misc.unopt model#get_iter_first) <> path
            then
              let iter            = model#get_iter path in
              let (table, cmodel) = model#get_data ~row:iter
              and tables_before   = fetch_tables_until iter in
              let constr_gui      =
                new vtable_constraints_gui
                  (self#_constraint_gui_answer cmodel)
                  (tables_before, [table])
              in
                constr_gui#start ()
            else
              GToolbox.message_box ~title:"Bad table selected in join"
                "Cannot add a constraint for the first table of the join"
        | _ ->
            GToolbox.message_box ~title:"No table selected in join"
              "Please, select a table in join"

  method private _cb__rem_constraint = fun () ->
      match constr_view.sv_view#selection#get_selected_rows with
        | [path] ->
            i_bool (current_constr_store#remove
                      (current_constr_store#get_iter path))
        | _ -> ()

  method private _cb__after_rem = fun row ->
    let (table, model) = select_w#store1#get_data ~row:row#iter in
      model#clear ();
      select_w#store2#foreach
        (fun _ iter ->
	   let (_, cmodel) = select_w#store2#get_data ~row:iter in
	     cmodel#clear_table_from_constraints (table.SQL_db.ta_name);
	     false)

  method private _cb__after_up = fun row_up row_down ->
    let (table_up, model_up)     = select_w#store2#get_data ~row:row_up#iter
    and (table_down, model_down) = select_w#store2#get_data ~row:row_down#iter in
      model_up#move_constraints_to table_down.SQL_db.ta_name model_down

  method private _cb__after_down = fun row_down row_up ->
    let (table_up, model_up)     = select_w#store2#get_data ~row:row_up#iter
    and (table_down, model_down) = select_w#store2#get_data ~row:row_down#iter in
      model_up#move_constraints_to table_down.SQL_db.ta_name model_down

  method private _cb__view_to_selection_changed = fun () ->
    match select_w#view2.SW.st_view#selection#get_selected_rows with
      | [path] ->
	  let iter = select_w#store2#get_iter path in
	  let (_, cmodel) = select_w#store2#get_data ~row:iter in
            self#_set_constr_model cmodel;
            glade#add_constr_button#misc#set_sensitive true
      | _ ->
          self#_set_constr_model empty_constr_store;
          glade#add_constr_button#misc#set_sensitive false

  method reset_data = fun name ->
    select_w#store1#clear ();
    select_w#store2#clear ();
    self#_set_constr_model empty_constr_store;
    glade#name_entry#set_text name

  method set_data_from_tables = fun tables name ->
    self#reset_data name;
    List.iter
      (fun t ->
         ignore (select_w#store1#append_data
                   (t, new constraints_list_store ())))
      tables

  method set_data_from_vtable = fun tables vtable ->
    self#reset_data vtable.SQL_db.vt_name;
    let tables_from =
      List.filter (fun t -> not (SQL_db.table_in_join vtable t)) tables
    in
      List.iter
        (fun t ->
           ignore (select_w#store1#append_data
                     (t, new constraints_list_store ())))
        tables_from;
      ignore (select_w#store2#append_data
                (vtable.SQL_db.vt_ftable, new constraints_list_store ()));
      List.iter
        (fun (t, columns) ->
           let c_store = new constraints_list_store () in
             List.iter
               (fun (c1, c2) ->
                  c_store#append_data { jc_column1 = c1; jc_column2 = c2; })
               columns;
             ignore (select_w#store2#append_data (t, c_store)))
        vtable.SQL_db.vt_join

  method start = fun () ->
    glade#toplevel#set_modal true;
    glade#toplevel#show ()

  initializer
  let handlers =
    [("on_cancel_button_clicked",     `Simple self#_cb__cancel_button_clicked);
     ("on_ok_button_clicked",         `Simple self#_cb__ok_button_clicked);
     ("on_add_constr_button_clicked", `Simple self#_cb__add_constraint);
     ("on_rem_constr_button_clicked", `Simple self#_cb__rem_constraint)]
  in
    select_w#connect#removed    (self#_cb__after_rem);
    select_w#connect#moved_up   (self#_cb__after_up);
    select_w#connect#moved_down (self#_cb__after_down);
    glade#cm_box#pack ~expand:true select_w#coerce;

    begin
      match defaults.vtgi_vtable with
        | Some vtable -> self#set_data_from_vtable tables vtable
        | None        ->
            self#set_data_from_tables tables defaults.vtgi_name
    end;

    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
    i (select_w#view2.SW.st_view#selection#connect#changed
         (self#_cb__view_to_selection_changed));
    i (constr_view.sv_view#selection#connect#changed
         (self#_cb__constr_view_selection_changed));

end
