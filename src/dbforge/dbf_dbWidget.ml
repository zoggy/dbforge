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

module WM = WidgetMisc

type data =
  | D_Table  of SQL_db.table
  | D_Column of SQL_db.column
  | D_String of string

type view = {
  sv_view     : GTree.view;
  sv_string_c : GTree.view_column;
}

class db_list_store () =
  let columns  = new GTree.column_list in
  let string_c = columns#add Gobject.Data.string in
  let color_c  = columns#add Gobject.Data.string in
  let caml_c   = columns#add Gobject.Data.caml in
  let tree_store = GTree.tree_store columns
  in
object (self)
  inherit GTree.tree_store
    (Gobject.try_cast tree_store#as_model "GtkTreeStore")

  method private _display_string = function
    | D_Table table   -> table.SQL_db.ta_name
    | D_Column column -> column.SQL_db.col_name
    | D_String s      -> s

  method set_data = fun c ~row ?color () ->
    begin match color with
      | None       -> ()
      | Some color -> self#set ~row ~column:color_c color;
    end;
    self#set ~row ~column:string_c (self#_display_string c);
    self#set ~row ~column:caml_c c

  method get_data = fun ~row ->
    self#get ~row ~column:caml_c

  method append_data = fun c ?parent ?color () ->
    let iter = self#append ?parent () in
      self#set_data c ~row:iter ?color ();
      iter

  method prepend_data = fun c ?parent ?color () ->
    let iter = self#prepend ?parent () in
      self#set_data c ~row:iter ?color ();
      iter

  method insert_data = fun i c ?parent ?color () ->
    let iter = self#insert i ?parent in
      self#set_data c ~row:iter ?color ();
      iter

  method create_view = fun ?(view = GTree.view ~headers_visible:false ()) () ->
    let name_column =
      GTree.view_column
	~renderer:(GTree.cell_renderer_text [],
		   [("text", string_c); ("foreground", color_c)])
	()
    in
      i_int (view#append_column name_column);
      view#set_model (Some self#coerce);
      { sv_view = view; sv_string_c = name_column; }
end

class db_widget mode =
  let model = new db_list_store () in
  let view  = model#create_view ()
  in
object (self)
  method view = view

  method set_tables = fun tables ->
    view.sv_view#set_model None;
    model#clear ();
    begin
      let append_columns = fun iter columns ->
	match columns with
	  | [] ->
	      ignore (model#append_data (D_String "No columns")
			~parent:iter ~color:"gray" ())
	  | columns ->
	      List.iter
		(fun c ->
		   ignore (model#append_data ~parent:iter (D_Column c) ()))
		columns
      in
	match tables with
	  | [] ->
	      ignore (model#append_data (D_String "No tables") ~color:"gray" ())
	  | _ ->
	      List.iter
		(fun t -> let iter = model#append_data (D_Table t) () in
		   append_columns iter (t.SQL_db.ta_columns))
		tables
    end;
    view.sv_view#set_model (Some model#coerce)

  method selected_columns =
    let pathes = view.sv_view#selection#get_selected_rows in
    let result =
      List.fold_left
        (fun acc path ->
           let iter = model#get_iter path in
           let data = model#get_data ~row:iter in
             match data with
               | D_Column c -> c :: acc
               | _          -> acc)
        []
        pathes
    in
      List.rev result

  initializer
    self#set_tables [];
    view.sv_view#selection#set_mode mode;
    view.sv_view#selection#set_select_function
      (fun path _ ->
         let iter = model#get_iter path in
           match model#get_data ~row:iter with
             | D_Column _ -> true
             | _          -> false)
end

let db_chooser tables mode =
  let dialog =
    GWindow.dialog
      ~title:"DB Column chooser" ~position:`MOUSE ()
  and db_view = new db_widget mode
  in let view_frame = GBin.frame
                        ~height:300 ~width:300
                        ~label:"Choose one column"
                        ~border_width:5
                        ~packing:dialog#vbox#add
                        ()
  in let scrolled_window = GBin.scrolled_window
                             ~hpolicy:`AUTOMATIC
                             ~vpolicy:`AUTOMATIC
                             ~border_width:5 ~shadow_type: `IN
                             ~packing:view_frame#add
                             ()
  in
    db_view#set_tables tables;
    scrolled_window#add db_view#view.sv_view#coerce;
    dialog#action_area#set_layout `START;
    dialog#add_button_stock `OK     `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    let rec run_dialog = fun () ->
      match dialog#run () with
        | `OK -> begin
            match db_view#selected_columns with
              | [] ->
                  GToolbox.message_box ~title:"Error"
                    "Please, choose a column in the list";
                  run_dialog ()
              | columns -> columns
          end
        | _   -> []
    in
    let result = run_dialog () in
      dialog#destroy ();
      result
