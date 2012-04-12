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

type table_gui_input = {
  tgi_name    : string;
  tgi_comment : string;
  tgi_logged  : bool ;
  tgi_pkey    : SQL_db.column list;
  tgi_columns : (SQL_db.column list) option;
}

class table_gui cb defaults =
  let glade  = new Dbf_gladeWidgets.table_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
  let pkey_w = new Dbf_selectWidget.select_and_order
                 ~label:"Primary key"
                 ~title1:"Out of primary key"
                 ~title2:"In primary key"
                 ~packing:(glade#cm_main_table#attach
                             ~expand:`BOTH
                             ~left:0 ~right:2 ~top:2)
                 ~extract:(fun c -> c.SQL_db.col_name)
                 ()
  in
object (self)
  method private _grab_input =
    let pkey =
      match pkey_w#store2#get_iter_first with
        | None      -> []
        | Some iter ->
            let rec grab = fun () ->
              let data = pkey_w#store2#get_data ~row:iter in
                if pkey_w#store2#iter_next iter then
                  data :: (grab ())
                else
                  [data]
            in
              grab ()
    in
    { tgi_name    = glade#name_entry#text;
      tgi_comment = glade#comments_textview#buffer#get_text ();
      tgi_logged  = glade#logged#active ;
      tgi_columns = defaults.tgi_columns;
      tgi_pkey    = pkey; }

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
    pkey_w#misc#set_sensitive (defaults.tgi_columns <> None);
    begin
      match defaults.tgi_columns with
        | None         -> ()
        | Some columns ->
            List.iter
              (fun c -> ignore (
                 if List.memq c defaults.tgi_pkey then
                   pkey_w#store2#append_data c
                 else
                   pkey_w#store1#prepend_data c))
              columns
    end;
    glade#name_entry#set_text defaults.tgi_name;
    glade#comments_textview#buffer#set_text defaults.tgi_comment;
    glade#logged#set_active defaults.tgi_logged;
    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml
end
