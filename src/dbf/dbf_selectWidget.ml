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
open Dbf_misc

module WM = WidgetMisc

type store_view = {
  st_view   : GTree.view;
  st_string : GTree.view_column;
}

class ['a] store ~extract =
  let columns  = new GTree.column_list in
  let c_string = columns#add Gobject.Data.string in
  let c_caml   = columns#add Gobject.Data.caml in
  let store    = GTree.list_store columns in
object(self)
  inherit GTree.list_store
    (Gobject.try_cast store#as_model "GtkListStore")

  method set_data = fun (data : 'a) ~row ->
    self#set ~row ~column:c_caml data;
    self#update_display row

  method get_data = fun ~row ->
    self#get ~row ~column:c_caml

  method append_data = fun data ->
    let iter = self#append () in
      self#set_data data ~row:iter;
      iter

  method prepend_data = fun data ->
    let iter = self#prepend () in
      self#set_data data ~row:iter;
      iter

  method insert_data = fun i data ->
    let iter = self#insert i in
      self#set_data data ~row:iter;
      iter

  method update_display = fun row ->
    let s = extract (self#get ~row:row ~column:c_caml) in
      self#set ~row:row ~column:c_string s

  method create_view = fun ?(view = GTree.view ()) ~title () ->
    let string_column =
      GTree.view_column ~title
        ~renderer:(GTree.cell_renderer_text [], [("text", c_string)])
        ()
    in
      string_column#set_resizable true;
      i_int (view#append_column string_column);
      view#set_model (Some store#coerce);
      { st_view   = view;
        st_string = string_column; }
end

class ['a] callbacks_poll =
object (self)
  val mutable cbs = ([] : 'a list)

  method add  = fun cb -> cbs <- cb :: cbs
  method iter = fun f ->
    List.iter f cbs
end

type added_cb_sig      = GTree.row_reference -> unit
type removed_cb_sig    = GTree.row_reference -> unit
type moved_up_cb_sig   = GTree.row_reference -> GTree.row_reference -> unit
type moved_down_cb_sig = GTree.row_reference -> GTree.row_reference -> unit

class select_and_order_connect =
object (self)
  val added_cbs      = (new callbacks_poll : (added_cb_sig callbacks_poll))
  val removed_cbs    = (new callbacks_poll : (removed_cb_sig callbacks_poll))
  val moved_up_cbs   = (new callbacks_poll : (moved_up_cb_sig callbacks_poll))
  val moved_down_cbs = (new callbacks_poll : (moved_down_cb_sig callbacks_poll))

  method added      = fun cb -> added_cbs#add cb
  method removed    = fun cb -> removed_cbs#add cb
  method moved_up   = fun cb -> moved_up_cbs#add cb
  method moved_down = fun cb -> moved_down_cbs#add cb

  method raise_added = fun row ->
    added_cbs#iter (fun cb -> cb row)

  method raise_removed = fun row ->
    removed_cbs#iter (fun cb -> cb row)

  method raise_moved_up = fun row1 row2 ->
    moved_up_cbs#iter (fun cb -> cb row1 row2)

  method raise_moved_down = fun row1 row2 ->
    moved_down_cbs#iter (fun cb -> cb row1 row2)
end

class ['a] select_and_order
    ?packing ~label ~title1 ~title2 ~(extract : 'a -> string) () =
  let store1 = new store ~extract
  and store2 = new store ~extract in
  let frame  = GBin.frame ~border_width:5 ?packing ~label () in
  let view1  = store1#create_view ~title:title1 ()
  and view2  = store2#create_view ~title:title2 ()
  in
object (self)
  inherit GObj.widget frame#as_widget

  val connect = new select_and_order_connect

  method store1  = store1
  method store2  = store2
  method view1   = view1
  method view2   = view2
  method connect = connect

  method private _cb__left_button_clicked = fun () ->
    match view2.st_view#selection#get_selected_rows with
      | [path] ->
          let iter = store2#get_iter path in
          let data = store2#get_data ~row:iter in
            i_bool (store2#remove iter);
            let row =
              store1#get_row_reference
                (store1#get_path (store1#prepend_data data))
            in
              if not (path_is_valid store2 path) then
                ignore(GTree.Path.prev path);
              view2.st_view#selection#select_path path;
              connect#raise_removed row
      | _ ->
          ()

  method private _cb__right_button_clicked = fun () ->
    match view1.st_view#selection#get_selected_rows with
      | [path] ->
          let iter = store1#get_iter path in
          let data = store1#get_data ~row:iter in
            i_bool (store1#remove iter);
            let row =
              store2#get_row_reference
                (store2#get_path (store2#append_data data))
            in
              if not (path_is_valid store1 path) then
                ignore(GTree.Path.prev path);
              view1.st_view#selection#select_path path;
              connect#raise_added row
      | _ ->
          ()

  method private _cb__up_button_clicked = fun () ->
    match view2.st_view#selection#get_selected_rows with
      | [path] ->
          let path_prev = GTree.Path.copy path in
            ignore (GTree.Path.prev path_prev);
            let i1 = GTree.Path.get_indices path
            and i2 = GTree.Path.get_indices path_prev in
              if i1 <> i2 then
                let row  = store2#get_row_reference path
                and row' = store2#get_row_reference path_prev in
                  i_bool (store2#swap row#iter row'#iter);
                  connect#raise_moved_up row row'
      | _ ->
          ()

  method private _cb__down_button_clicked = fun () ->
    match view2.st_view#selection#get_selected_rows with
      | [path] ->
          let path_next = GTree.Path.copy path in
            GTree.Path.next path_next;
            if path_is_valid store2 path_next then
              let row  = store2#get_row_reference path
              and row' = store2#get_row_reference path_next in
                i_bool (store2#swap row#iter row'#iter);
                connect#raise_moved_down row row'
      | _ ->
          ()

  method private _cb__selection_changed = fun view buttons () ->
    let sth_selected = view.st_view#selection#get_selected_rows <> [] in
      List.iter (fun b -> b#misc#set_sensitive sth_selected) buttons

  initializer
  let sep1  = GMisc.separator `VERTICAL ()
  and sep2  = GMisc.separator `VERTICAL ()
  and sep3  = GMisc.separator `VERTICAL ()
  and sw1   = GBin.scrolled_window
                ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ()
  and sw2   = GBin.scrolled_window
                ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN ()
  and bbox1 = GPack.button_box `VERTICAL
                ~spacing:10 ~border_width:5 ~layout:`START ()
  and bbox2 = GPack.button_box `VERTICAL
                ~spacing:10 ~border_width:5 ~layout:`START ()
  and hbox  = GPack.hbox
                ~packing:frame#add ~border_width:5 ~spacing:10 ()
  in
  let left_button  = WM.create_button ~stock:`GO_BACK    ~packing:bbox1#pack () in
  let right_button = WM.create_button ~stock:`GO_FORWARD ~packing:bbox1#pack () in
  let up_button    = WM.create_button ~stock:`GO_UP      ~packing:bbox2#pack () in
  let down_button  = WM.create_button ~stock:`GO_DOWN    ~packing:bbox2#pack ()
  in
    sw1#add view1.st_view#coerce;
    sw2#add view2.st_view#coerce;
    hbox#pack ~expand:true  sw1#coerce;
    hbox#pack ~expand:false sep1#coerce;
    hbox#pack ~expand:false bbox1#coerce;
    hbox#pack ~expand:false sep2#coerce;
    hbox#pack ~expand:true  sw2#coerce;
    hbox#pack ~expand:false sep3#coerce;
    hbox#pack ~expand:false bbox2#coerce;

    List.iter (fun b -> b#misc#set_sensitive false)
      [left_button; right_button; up_button; down_button];

    i (left_button#connect#clicked  self#_cb__left_button_clicked);
    i (right_button#connect#clicked self#_cb__right_button_clicked);
    i (up_button#connect#clicked    self#_cb__up_button_clicked);
    i (down_button#connect#clicked  self#_cb__down_button_clicked);

    i (view1.st_view#selection#connect#changed
         (self#_cb__selection_changed view1
            [right_button]));
    i (view2.st_view#selection#connect#changed
         (self#_cb__selection_changed view2
            [left_button; up_button; down_button]));

    store1#set_sort_column_id 0 `ASCENDING
end
