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

(*===================\
| Strings list store |
\===================*)
type string_view = {
  str_view   : GTree.view;
  str_string : GTree.view_column;
}

class string_list_store () =
  let columns    = new GTree.column_list in
  let c_string   = columns#add Gobject.Data.string in
  let list_store = GTree.list_store columns in
object (self)
  inherit GTree.list_store
    (Gobject.try_cast list_store#as_model "GtkListStore")

  method set_data = fun data ~row ->
    self#set ~row ~column:c_string data;

  method get_data = fun ~row ->
    self#get ~row ~column:c_string

  method append_data = fun data ->
    let iter = self#append () in
      self#set_data data ~row:iter

  method prepend_data = fun data ->
    let iter = self#prepend () in
      self#set_data data ~row:iter

  method insert_data = fun i data ->
    let iter = self#insert i in
      self#set_data data ~row:iter

  method create_view = fun ?(view = GTree.view ()) ~title () ->
    let string_column =
      GTree.view_column ~title
	~renderer:(GTree.cell_renderer_text [], [("text", c_string)])
	()
    in
      string_column#set_resizable true;
      i_int (view#append_column string_column);
      view#set_model (Some self#coerce);
      { str_view   = view;
        str_string = string_column; }
end

type string_2_view = {
  str2_view    : GTree.view;
  str2_string1 : GTree.view_column;
  str2_string2 : GTree.view_column;
}

class string2_list_store () =
  let columns    = new GTree.column_list in
  let c_string1  = columns#add Gobject.Data.string in
  let c_string2  = columns#add Gobject.Data.string in
  let list_store = GTree.list_store columns in
object (self)
  inherit GTree.list_store
    (Gobject.try_cast list_store#as_model "GtkListStore")

  method set_data = fun (s1, s2) ~row ->
    self#set ~row ~column:c_string1 s1;
    self#set ~row ~column:c_string2 s2

  method get_data = fun ~row ->
    (self#get ~row ~column:c_string1,
     self#get ~row ~column:c_string2)

  method append_data = fun data ->
    let iter = self#append () in
      self#set_data data ~row:iter

  method prepend_data = fun data ->
    let iter = self#prepend () in
      self#set_data data ~row:iter

  method insert_data = fun i data ->
    let iter = self#insert i in
      self#set_data data ~row:iter

  method create_view = fun ?(view = GTree.view ()) ~title1 ~title2 () ->
    let string1_column =
      GTree.view_column ~title:title1
	~renderer:(GTree.cell_renderer_text [], [("text", c_string1)])
	()
    and string2_column =
      GTree.view_column ~title:title2
	~renderer:(GTree.cell_renderer_text [], [("text", c_string2)])
	()
    in
      List.iter
        (fun c ->
           c#set_resizable true;
           i_int (view#append_column c))
        [string1_column; string2_column];
      view#set_model (Some self#coerce);
      { str2_view    = view;
        str2_string1 = string1_column;
        str2_string2 = string2_column; }

end

(*==============\
| Columns store |
\==============*)
type column_view = {
  col_view     : GTree.view;
  col_name     : GTree.view_column;
  col_nullable : GTree.view_column;
  col_type     : GTree.view_column;
  col_ocaml_ty : GTree.view_column;
  col_options  : GTree.view_column;
}

class column_store () =
  let columns    = new GTree.column_list in
  let c_string   = columns#add Gobject.Data.string in
  let c_nullable = columns#add Gobject.Data.string in
  let c_type     = columns#add Gobject.Data.string in
  let c_ocaml_ty = columns#add Gobject.Data.string in
  let c_options  = columns#add Gobject.Data.string in
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
    let column = self#get_data ~row:iter in
      self#set ~row:iter ~column:c_string (column.SQL_db.col_name);
      self#set ~row:iter ~column:c_nullable
        (if column.SQL_db.col_nullable then "yes" else "no");
      begin
        let ty_s = SQL_ty.fullstring_of_type column.SQL_db.col_type
        and spec_tys_s =
          let strings =
            Dbf_misc.StringMap.fold
              (fun db ty acc -> (Printf.sprintf "%s: %s" db ty) :: acc)
              column.SQL_db.col_spec_ty []
          in
            Dbf_misc.join ~sep:", " ~to_string:(fun x -> x)
              strings
        in
          if not (Dbf_misc.StringMap.is_empty column.SQL_db.col_spec_ty) then
            self#set ~row:iter ~column:c_type
              (Printf.sprintf "%s [%s]" ty_s spec_tys_s)
          else
            self#set ~row:iter ~column:c_type ty_s
      end;
      self#set ~row:iter ~column:c_ocaml_ty
        (column.SQL_db.col_ocaml_ty);
      self#set ~row:iter ~column:c_options
        (SQL_db.string_of_spec_options column.SQL_db.col_spec_options)

  method create_view = fun ?(view = GTree.view ()) () ->
    let name_column =
      GTree.view_column ~title:"Name"
	~renderer:(GTree.cell_renderer_text [], [("text", c_string)])
	()
    and nullable_column =
      GTree.view_column ~title:"Nullable ?"
        ~renderer:(GTree.cell_renderer_text [], [("text", c_nullable)])
        ()
    and type_column =
      GTree.view_column ~title:"Type"
        ~renderer:(GTree.cell_renderer_text [], [("text", c_type)])
        ()
    and ocaml_ty_column =
      GTree.view_column ~title:"OCaml type"
        ~renderer:(GTree.cell_renderer_text [], [("text", c_ocaml_ty)])
        ()
    and options_column =
      GTree.view_column ~title:"Option"
        ~renderer:(GTree.cell_renderer_text [], [("text", c_options)])
        ()
    in
      List.iter
        (fun c ->
           c#set_resizable true;
           i_int (view#append_column c))
        [name_column; nullable_column; type_column;
         ocaml_ty_column; options_column];
      view#set_model (Some self#coerce);
      { col_view     = view;
        col_name     = name_column;
        col_nullable = nullable_column;
        col_type     = type_column;
        col_ocaml_ty = ocaml_ty_column;
        col_options  = options_column; }

  initializer
    self#set_sort_column_id 0 `ASCENDING
end

(*=============\
| Tables store |
\=============*)
type table_view = {
  tv_view : GTree.view;
  tv_name : GTree.view_column;
  tv_pkey : GTree.view_column;
}

type table_data = {
  td_table : SQL_db.table;
  td_model : column_store;
}

class table_store () =
  let columns    = new GTree.column_list in
  let c_string   = columns#add Gobject.Data.string in
  let c_pkey     = columns#add Gobject.Data.string in
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
    let data = self#get_data ~row:iter in
      self#set ~row:iter ~column:c_string (data.td_table.SQL_db.ta_name);
      self#set ~row:iter ~column:c_pkey
        (Dbf_misc.join ~sep:", " ~to_string:(fun c -> c.SQL_db.col_name)
           data.td_table.SQL_db.ta_pkey)

  method create_view = fun ?(view = GTree.view ()) () ->
    let name_column =
      GTree.view_column ~title:"Name"
	~renderer:(GTree.cell_renderer_text [], [("text", c_string)])
	()
    and pkey_column =
      GTree.view_column ~title:"Primary key"
	~renderer:(GTree.cell_renderer_text [], [("text", c_pkey)])
	()
    in
      List.iter
        (fun c ->
           c#set_resizable true;
           i_int (view#append_column c))
        [name_column; pkey_column];
      view#set_model (Some self#coerce);
      { tv_view = view;
        tv_name = name_column;
        tv_pkey = pkey_column; }

  initializer
    self#set_sort_column_id 0 `ASCENDING
end

(*=====================\
| Virtual tables store |
\=====================*)
type vtable_view = {
  vt_view  : GTree.view;
  vt_name  : GTree.view_column;
  vt_descr : GTree.view_column;
}

class vtable_store () =
  let columns    = new GTree.column_list in
  let c_name     = columns#add Gobject.Data.string in
  let c_descr    = columns#add Gobject.Data.string in
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
    let vtable = self#get_data ~row:iter in
      self#set ~row:iter ~column:c_name  (vtable.SQL_db.vt_name);
      self#set ~row:iter ~column:c_descr (SQL_db.string_of_vtable vtable)

  method create_view = fun ?(view = GTree.view ()) () ->
    let name_vtable =
      GTree.view_column ~title:"Name"
	~renderer:(GTree.cell_renderer_text [], [("text", c_name)])
	()
    and descr_column =
      GTree.view_column ~title:"Virtual table description"
	~renderer:(GTree.cell_renderer_text [], [("text", c_descr)])
	()
    in
      name_vtable#set_resizable true;
      descr_column#set_resizable true;
      i_int (view#append_column name_vtable);
      i_int (view#append_column descr_column);
      view#set_model (Some self#coerce);
      { vt_view  = view;
        vt_name  = name_vtable;
        vt_descr = descr_column}

  initializer
    self#set_sort_column_id 0 `ASCENDING
end

(*==============\
| Indexes store |
\==============*)
type index_view = {
  idx_view   : GTree.view;
  idx_name   : GTree.view_column;
  idx_unique : GTree.view_column;
  idx_table  : GTree.view_column;
  idx_descr  : GTree.view_column;
}

class index_store () =
  let columns    = new GTree.column_list in
  let c_name     = columns#add Gobject.Data.string in
  let c_table    = columns#add Gobject.Data.string in
  let c_descr    = columns#add Gobject.Data.string in
  let c_unique   = columns#add Gobject.Data.string in
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
    let index = self#get_data ~row:iter in
      self#set ~row:iter ~column:c_name (index.SQL_db.idx_name);
      self#set ~row:iter ~column:c_unique
        (if index.SQL_db.idx_unique then "yes" else "no");
      self#set ~row:iter ~column:c_table
        (SQL_db.table_of_index index).SQL_db.ta_name;
      self#set ~row:iter ~column:c_descr (SQL_db.string_of_index index)

  method create_view = fun ?(view = GTree.view ()) () ->
    let result = {
      idx_view = view;
      idx_name =
        GTree.view_column ~title:"Name"
	  ~renderer:(GTree.cell_renderer_text [], [("text", c_name)])
	  ();
      idx_unique =
        GTree.view_column ~title:"Unique ?"
          ~renderer:(GTree.cell_renderer_text [], [("text", c_unique)])
          ();
      idx_table =
        GTree.view_column ~title:"Index on table"
	  ~renderer:(GTree.cell_renderer_text [], [("text", c_table)])
	  ();
      idx_descr =
        GTree.view_column ~title:"Index description"
	  ~renderer:(GTree.cell_renderer_text [], [("text", c_descr)])
	  ();
    } in
      List.iter
        (fun v -> v#set_resizable true; i_int (view#append_column v))
        [result.idx_name; result.idx_unique;
         result.idx_table; result.idx_descr];
      view#set_model (Some self#coerce);
      result

  initializer
    self#set_sort_column_id 0 `ASCENDING
end

(*==============\
| Queries store |
\==============*)
type query_view = {
  qry_view    : GTree.view;
  qry_name    : GTree.view_column;
  qry_query   : GTree.view_column;
  qry_state   : GTree.view_column;
}

class query_store () =
  let columns    = new GTree.column_list in
  let c_name     = columns#add Gobject.Data.string in
  let c_query    = columns#add Gobject.Data.string in
  let c_state    = columns#add Gobject.Data.string in
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
    let query = self#get_data ~row:iter in
    self#set ~row:iter ~column:c_name (query.SQL_db.qry_name);
    self#set ~row:iter ~column:c_query (query.SQL_db.qry_query);
    let state = SQL_db.query_state query in
    self#set ~row:iter ~column:c_state (SQL_db.string_of_query_state state)

  method create_view = fun ?(view = GTree.view ()) () ->
    let result = {
      qry_view = view;
      qry_name =
        GTree.view_column ~title:"Name"
	  ~renderer:(GTree.cell_renderer_text [], [("text", c_name)])
	  ();
      qry_query =
        GTree.view_column ~title:"Query"
	  ~renderer:(GTree.cell_renderer_text [], [("text", c_query)])
	  ();
      qry_state =
        GTree.view_column ~title:"State"
          ~renderer:(GTree.cell_renderer_text [], [("text", c_state)])
          ();
    } in
      List.iter
        (fun v -> v#set_resizable true; i_int (view#append_column v))
        [result.qry_name; result.qry_query; result.qry_state;];
      view#set_model (Some self#coerce);
      result

  initializer
    self#set_sort_column_id 0 `ASCENDING
end
