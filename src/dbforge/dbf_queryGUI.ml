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

type query_gui_input = {
  qrygi_name    : string;
  qrygi_query   : string;
  qrygi_comment : string;
}

class query_gui cb f_check (defaults : query_gui_input) =
  let glade      = new Dbf_gladeWidgets.query_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
(*
  let qry_select = new Dbf_selectWidget.select_and_order
                     ~label:"Columns in index"
                     ~title1:"Out of index"
                     ~title2:"In index"
                     ~extract:(fun c -> c.SQL_db.col_name)
                     ~packing:(glade#cm_main_table#attach
                                 ~left:0 ~right:2 ~top:3
                                 ~expand:`BOTH)
                     ()
  in
*)
object (self)
  method private _grab_input =
    { qrygi_name    = Dbf_misc.strip_string (glade#name_entry#text);
      qrygi_query   = Dbf_misc.strip_string (glade#query_view#buffer#get_text ());
      qrygi_comment = Dbf_misc.strip_string (glade#comment_view#buffer#get_text ());
    }

  (* Callbacks *)
  method private _cb__ok_button_clicked = fun () ->
    try
      let t = self#_grab_input in
      f_check t;
      cb t;
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
	("on_cancel_button_clicked", `Simple self#_cb__cancel_button_clicked);
      ]
    in
    glade#name_entry#set_text defaults.qrygi_name;
    glade#query_view#buffer#set_text defaults.qrygi_query;
    glade#comment_view#buffer#set_text defaults.qrygi_comment;
    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
end
