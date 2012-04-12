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

let i = fun (_ : GtkSignal.id) -> ()

let path_is_valid = fun store path ->
  try  let r = store#get_row_reference path in r#valid
  with Gpointer.Null -> false

module WidgetMisc =
struct
  let create_button = fun ?label ~stock ~packing () ->
    let button = GButton.button ~packing () in
      match label with
	| Some label ->
	    let align = GBin.alignment
			  ~xalign:0.5 ~yalign:0.5
			  ~xscale:0.0 ~yscale:0.0
			  ~packing:button#add
			  ()
	    in
	    let bbox = GPack.hbox ~spacing:2 ~packing:align#add () in
	      ignore (GMisc.image ~stock:stock ~packing:bbox#pack ());
	      ignore (GMisc.label ~text:label ~packing:bbox#pack ());
	      button
	| None ->
	    ignore (GMisc.image ~stock:stock ~packing:button#add ());
	    button
end
