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
open Dbf_dbStores

type sopts_view = {
  sopt_view   : GTree.view;
  sopt_string : GTree.view_column;
}

class specific_opts_store () =
  let columns    = new GTree.column_list in
  let c_string   = columns#add Gobject.Data.string in
  let tree_store = GTree.tree_store columns in
object (self)
  inherit GTree.tree_store
    (Gobject.try_cast tree_store#as_model "GtkTreeStore")

  method set_data = fun s ~row ->
    self#set ~row ~column:c_string s

  method get_data = fun ~row ->
    self#get ~row ~column:c_string

  method append_data = fun ?parent data ->
    let iter = self#append ?parent () in
      self#set_data data ~row:iter;
      iter

  method prepend_data = fun ?parent data ->
    let iter = self#prepend ?parent () in
      self#set_data data ~row:iter;
      iter

  method insert_data = fun ?parent i data ->
    let iter = self#insert ?parent i in
      self#set_data data ~row:iter;
      iter

  method create_view = fun ?(view = GTree.view ()) () ->
    let string_column =
      GTree.view_column ~title:"Database/Option"
	~renderer:(GTree.cell_renderer_text [], [("text", c_string)])
	()
    in
      List.iter
        (fun c ->
           c#set_resizable true;
           i_int (view#append_column c))
        [string_column];
      view#set_model (Some self#coerce);
      { sopt_view   = view;
        sopt_string = string_column; }

end

let ask_for_a_specific_option = fun ?parent name ->
  let dialog = GWindow.dialog ~title:"New option" ?parent ()
  in
  let main_box =
    GPack.table
      ~columns:2 ~rows:2
      ~row_spacings:5 ~col_spacings:10
      ~packing:(dialog#vbox#pack ~expand:true)
      ~border_width:5
      ()
  in
    ignore (GMisc.label ~text:"Database" ~xalign:0.
              ~packing:(main_box#attach ~top:0 ~left:0 ~expand:`NONE ~fill:`X)
              ());
    ignore (GMisc.label ~text:name ~xalign:0.
              ~packing:(main_box#attach ~top:1 ~left:0 ~expand:`NONE ~fill:`X)
              ());
    let db_entry =
      GEdit.entry
        ~packing:(main_box#attach ~top:0 ~left:1 ~expand:`X ~fill:`X) ()
    and opt_entry =
      GEdit.entry
        ~packing:(main_box#attach ~top:1 ~left:1 ~expand:`X ~fill:`X) ()
    in
      dialog#action_area#set_layout `START;
      dialog#add_button_stock `OK `OK;
      dialog#add_button_stock `CANCEL `CANCEL;
      let rec ask_for_option = fun () ->
        match dialog#run () with
          | `OK ->
              let db  = Dbf_misc.trim db_entry#text
              and opt = Dbf_misc.trim opt_entry#text in
                if db = "" || opt = "" then begin
                  GToolbox.message_box ~title:"Invalid input"
                    "You must fill all the fields";
                  ask_for_option ()
                end else begin
                  dialog#destroy ();
                  Some (db, opt)
                end
          | _   ->
              dialog#destroy ();
              None
      in
        ask_for_option ()


type column_gui_type = {
  cgi_name       : string;
  cgi_comment    : string;
  cgi_type       : SQL_db.ty;
  cgi_nullable   : bool;
  cgi_options    : (string list) Dbf_misc.StringMap.t;
  cgi_spec_ty    : string Dbf_misc.StringMap.t;
  cgi_ocaml_type : string;
  cgi_sql2ml     : string;
  cgi_ml2sql     : string;
}

class column_gui cb defaults =
  let glade = new Dbf_gladeWidgets.column_window
      ~file: Dbf_installation.glade_file
      ~autoconnect:false () in
  let sopts_store  = new specific_opts_store () in
  let sopts_view   = sopts_store#create_view ~view:glade#spec_opts_view () in
  let tyopts_store = new string2_list_store () in
  let tyopts_view  = tyopts_store#create_view
                       ~view:glade#spec_ty_view
                       ~title1:"Database" ~title2:"Type"
                       ()
  in
object (self)
  method private _get_dispsize =
    match glade#dispsize_check#active with
      | true  -> Some glade#dispsize_spinbutton#value_as_int
      | false -> None

  method private _get_precision =
    match glade#prec_check#active with
      | true  -> Some glade#prec_spinbutton#value_as_int
      | false -> None

  method private _grab_input =
    let ty = SQL_ty.type_of_string
               ?dispsize:  self#_get_dispsize
               ?precision: self#_get_precision
               ~options:   glade#other_opts_combo#entry#text
               glade#ty_combo#entry#text
    and options =
      let fetch_strings = fun parent ->
        if sopts_store#iter_has_child parent then
          let iter = sopts_store#iter_children (Some parent) in
          let result = ref [sopts_store#get_data ~row:iter] in
            while sopts_store#iter_next iter do
              result := (sopts_store#get_data ~row:iter) :: !result
            done;
            List.rev !result
        else
          []
      in
        match sopts_store#get_iter_first with
          | None ->
              Dbf_misc.StringMap.empty
          | Some iter ->
              let rec fetch = fun map ->
                let db   = sopts_store#get_data ~row:iter
                and opts = fetch_strings iter in
                let map  = Dbf_misc.StringMap.add db opts map in
                  match sopts_store#iter_next iter with
                    | true  -> fetch map
                    | false -> map
              in
                fetch Dbf_misc.StringMap.empty

    and spec_ty =
      match tyopts_store#get_iter_first with
        | None ->
            Dbf_misc.StringMap.empty
        | Some iter ->
            let rec fetch = fun map ->
              let (db, ty) = tyopts_store#get_data ~row:iter in
              let map      = Dbf_misc.StringMap.add db ty map in
                match tyopts_store#iter_next iter with
                  | true  -> fetch map
                  | false -> map
            in
              fetch Dbf_misc.StringMap.empty
    in
      { cgi_name       = glade#name_entry#text;
        cgi_comment    = glade#comments_textview#buffer#get_text ();
        cgi_type       = ty;
        cgi_nullable   = glade#nullable_check#active;
        cgi_options    = options;
        cgi_spec_ty    = spec_ty;
        cgi_ocaml_type = glade#ocamlty_combo#entry#text;
        cgi_sql2ml     = glade#sql2ml_combo#entry#text;
        cgi_ml2sql     = glade#ml2sql_combo#entry#text; }

  (* Callbacks *)
  method private _cb__dispsize_check_toggled = fun () ->
    glade#dispsize_spinbutton#misc#set_sensitive
    glade#dispsize_check#active

  method private _cb__prec_check_toggled = fun () ->
    glade#prec_spinbutton#misc#set_sensitive
    glade#prec_check#active

  method private _cb__ok_button_clicked = fun () ->
    try
      cb self#_grab_input;
      glade#toplevel#destroy ()
    with
      | Invalid_input msg ->
          GToolbox.message_box ~title:"Invalid input" msg
      | SQL_ty.Invalid_type msg ->
          GToolbox.message_box ~title:"Invalid type" msg

  method private _cb__cancel_button_clicked = fun () ->
    glade#toplevel#destroy ()

  method private _cb__add_opt_button_clicked = fun () ->
    match ask_for_a_specific_option ~parent:glade#toplevel "Option" with
      | None      -> ()
      | Some data ->
          self#add_new_option data

  method private _cb__remove_opt_button_clicked = fun () ->
    let select_around = fun path ->
      let rec select_around = fun () ->
        if not (path_is_valid sopts_store path) then begin
          ignore(GTree.Path.prev path);
          if not (path_is_valid sopts_store path) then begin
            if GTree.Path.get_depth path > 1 then begin
              i_bool (GTree.Path.up path);
              i_bool (sopts_store#remove (sopts_store#get_iter path));
              select_around ()
            end
          end
        end
      in
        select_around ();
        if GTree.Path.get_depth path > 0 then
          sopts_view.sopt_view#selection#select_path path
    in
      match sopts_view.sopt_view#selection#get_selected_rows with
        | [path] ->
            let iter = sopts_store#get_iter path in
              i_bool (sopts_store#remove iter);
              select_around path
        | _ ->
            ()

  method private _cb__up_opt_button_clicked = fun () ->
    match sopts_view.sopt_view#selection#get_selected_rows with
      | [path] ->
          if GTree.Path.get_depth path > 1 then
            let path_prev = GTree.Path.copy path in
              ignore(GTree.Path.prev path_prev);
              i_bool (sopts_store#swap
                        (sopts_store#get_iter path)
                        (sopts_store#get_iter path_prev))
      | _ ->
          ()

  method private _cb__down_opt_button_clicked = fun () ->
    match sopts_view.sopt_view#selection#get_selected_rows with
      | [path] ->
          if GTree.Path.get_depth path > 1 then
            let path_next = GTree.Path.copy path in
              GTree.Path.next path_next;
              if path_is_valid sopts_store path_next then
                i_bool (sopts_store#swap
                          (sopts_store#get_iter path)
                          (sopts_store#get_iter path_next))
      | _ ->
          ()

  method private _cb__add_ty_button_clicked = fun () ->
    match ask_for_a_specific_option ~parent:glade#toplevel "Type" with
      | None          -> ()
      | Some (db, ty) ->
          self#add_new_spec_type db ty

  method private _cb__remove_ty_button_clicked = fun () ->
    match tyopts_view.str2_view#selection#get_selected_rows with
      | [path] ->
          let iter = tyopts_store#get_iter path in
            ignore (tyopts_store#remove iter);
            if not (path_is_valid tyopts_store path) then
              ignore(GTree.Path.prev path);
            tyopts_view.str2_view#selection#select_path path
      | _ ->
          ()

  method private _cb__ty_changed = fun (item : GList.list_item) ->
    let ty = SQL_ty.kind_of_string glade#ty_combo#entry#text in
      begin
        let do_dispsize = SQL_ty.kind_uses_display_width ty in
          glade#dispsize_check#misc#set_sensitive (do_dispsize = Maybe);
          if do_dispsize = No then
            glade#dispsize_check#set_active false
          else if do_dispsize = Yes then
            glade#dispsize_check#set_active true
      end;
      begin
        let do_prec = SQL_ty.kind_uses_precision ty in
          glade#prec_check#misc#set_sensitive (do_prec = Maybe);
          if do_prec = No then
            glade#prec_check#set_active false
          else if do_prec = Yes then
            glade#prec_check#set_active true
      end;
      begin
        let options = SQL_ty.options_of_kind ty
        and current_option = glade#other_opts_combo#entry#text in
          glade#other_opts_combo#list#clear_items ~start:0 ~stop:(-1);
          List.iter
            (fun s -> glade#other_opts_combo#list#insert ~pos:(-1)
               (GList.list_item ~label:s ()))
            options;
          if List.mem current_option options then
            glade#other_opts_combo#entry#set_text current_option
          else match options with
            | []      -> glade#other_opts_combo#entry#set_text ""
            | hd :: _ -> glade#other_opts_combo#entry#set_text hd
      end

  method add_new_option = fun (db, opt) ->
    let do_insert = fun () ->
      match sopts_store#get_iter_first with
        | None      ->
            let iter = sopts_store#prepend_data db in
              (iter, sopts_store#prepend_data ~parent:iter opt)
        | Some iter ->
            let rec insert = fun () ->
              let db' = sopts_store#get_data ~row:iter in
                if db = db' then
                  (iter, sopts_store#append_data ~parent:iter opt)
                else if db < db' then
                  let iter' = sopts_store#insert_before iter in
                    sopts_store#set_data db ~row:iter';
                    (iter', sopts_store#prepend_data ~parent:iter' opt)
                else if sopts_store#iter_next iter then
                  insert ()
                else
                  let iter' = sopts_store#insert_before iter in
                    sopts_store#set_data db ~row:iter';
                    (iter', sopts_store#prepend_data ~parent:iter' opt)
            in
              insert ()
    in
    let (iter, iter_c) = do_insert () in
    let path   = sopts_store#get_path iter
    and path_c = sopts_store#get_path iter_c in
      sopts_view.sopt_view#expand_row ~all:false path;
      sopts_view.sopt_view#scroll_to_cell path_c sopts_view.sopt_string;
      sopts_view.sopt_view#selection#select_path path_c

  method add_new_spec_type = fun db ty ->
    let row = ref None in
      tyopts_store#foreach
        (fun path iter ->
           let (db', _) = tyopts_store#get_data ~row:iter in
             if db = db' then begin
               row := Some (tyopts_store#get_row_reference path);
               true
             end else
               false);
      match !row with
        | Some r ->
            tyopts_store#set_data (db, ty) ~row:r#iter
        | None   ->
            tyopts_store#prepend_data (db, ty)

  method start = fun () ->
    glade#toplevel#set_modal true;
    glade#toplevel#show ()

  initializer
  let handlers =
    [("on_ok_button_clicked",        `Simple self#_cb__ok_button_clicked);
     ("on_cancel_button_clicked",    `Simple self#_cb__cancel_button_clicked);
     ("on_add_button_clicked",       `Simple self#_cb__add_opt_button_clicked);
     ("on_remove_button_clicked",    `Simple self#_cb__remove_opt_button_clicked);
     ("on_up_button_clicked",        `Simple self#_cb__up_opt_button_clicked);
     ("on_down_button_clicked",      `Simple self#_cb__down_opt_button_clicked);
     ("on_add_ty_button_clicked",    `Simple self#_cb__add_ty_button_clicked);
     ("on_remove_ty_button_clicked", `Simple self#_cb__remove_ty_button_clicked);
     ("on_dispsize_check_toggled",   `Simple self#_cb__dispsize_check_toggled);
     ("on_prec_check_toggled",       `Simple self#_cb__prec_check_toggled)]
  in
    List.iter
      (fun (_, s) ->
         glade#ty_combo#list#insert
         (GList.list_item ~label:s ()) ~pos:(-1))
      SQL_ty.kind_string_assoc;

    Glade.bind_handlers ~extra:handlers ~warn:true glade#xml;
    i (glade#ty_combo#list#connect#select_child ~callback:self#_cb__ty_changed);

    tyopts_store#set_sort_column_id 0 `ASCENDING;

    glade#name_entry#set_text defaults.cgi_name;
    glade#comments_textview#buffer#set_text defaults.cgi_comment;
    glade#ty_combo#entry#set_text (SQL_ty.string_of_type defaults.cgi_type);
    begin match SQL_ty.string_of_type_options defaults.cgi_type with
      | None   -> ()
      | Some s -> glade#other_opts_combo#entry#set_text s
    end;
    Dbf_misc.StringMap.iter
      (fun db options ->
         let iter = sopts_store#append_data db in
           List.iter
             (fun option ->
                ignore (sopts_store#append_data ~parent:iter option))
             options)
      defaults.cgi_options;
    Dbf_misc.StringMap.iter
      (fun db ty -> tyopts_store#prepend_data (db, ty))
      defaults.cgi_spec_ty;
    glade#nullable_check#set_active defaults.cgi_nullable;
    glade#ocamlty_combo#entry#set_text defaults.cgi_ocaml_type;
    glade#sql2ml_combo#entry#set_text defaults.cgi_sql2ml;
    glade#ml2sql_combo#entry#set_text defaults.cgi_ml2sql;

    begin
      match SQL_ty.get_display_size defaults.cgi_type with
        | None   -> ()
        | Some i ->
            glade#dispsize_check#set_active true;
            glade#dispsize_spinbutton#set_value (float_of_int i)
    end;
    begin
      match SQL_ty.get_precision defaults.cgi_type with
        | None   -> ()
        | Some i ->
            glade#prec_check#set_active true;
            glade#prec_spinbutton#set_value (float_of_int i)
    end

end
