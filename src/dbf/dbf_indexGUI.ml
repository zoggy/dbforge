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

type table_gui_input = {
  idxgi_name    : string;
  idxgi_table   : SQL_db.table;
  idxgi_columns : SQL_db.column list;
  idxgi_unique  : bool;
}

class index_gui cb (defaults : table_gui_input) =
  let glade      = new Dbf_gladeWidgets.index_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
  let idx_select = new Dbf_selectWidget.select_and_order
                     ~label:"Columns in index"
                     ~title1:"Out of index"
                     ~title2:"In index"
                     ~extract:(fun c -> c.SQL_db.col_name)
                     ~packing:(glade#cm_main_table#attach
                                 ~left:0 ~right:2 ~top:3
                                 ~expand:`BOTH)
                     ()
  in
object (self)
  method private _grab_input =
    { idxgi_name    = glade#name_entry#text;
      idxgi_table   = defaults.idxgi_table;
      idxgi_columns = self#columns_in_index;
      idxgi_unique  = glade#unique_check#active;
    }

  method set_columns = fun (columns1, columns2) ->
    idx_select#store1#clear ();
    idx_select#store2#clear ();
    List.iter
      (fun c -> ignore (idx_select#store1#prepend_data c))
      columns1;
    List.iter
      (fun c -> ignore (idx_select#store2#prepend_data c))
      columns2

  method columns_in_index =
    match idx_select#store2#get_iter_first with
      | None      -> []
      | Some iter ->
          let rec fetch = fun () ->
            let data = idx_select#store2#get_data ~row:iter in
              if idx_select#store2#iter_next iter then
                data :: (fetch ())
              else
                [data]
          in
            fetch ()

  (* Callbacks *)
  method private _cb__ok_button_clicked = fun () ->
    try
      cb self#_grab_input;
      glade#toplevel#destroy ()
    with
      | Invalid_input msg ->
          GToolbox.message_box ~title:"Invalid input" msg

  method private _cb__cancel_button_clicked = fun () ->
    glade#toplevel#destroy ()

  method start = fun () ->
    glade#toplevel#set_modal true;
    glade#toplevel#show ()

  initializer
  let handlers =
    [("on_ok_button_clicked",     `Simple self#_cb__ok_button_clicked);
     ("on_cancel_button_clicked", `Simple self#_cb__cancel_button_clicked)]
  in
    self#set_columns
      (List.partition
         (fun c -> not (List.memq c defaults.idxgi_columns))
         defaults.idxgi_table.SQL_db.ta_columns);
    glade#tablename_entry#set_text defaults.idxgi_table.SQL_db.ta_name;
    glade#unique_check#set_active defaults.idxgi_unique;
    glade#name_entry#set_text defaults.idxgi_name;
    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
end
