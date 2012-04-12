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

(** This module defines a class used to edit a dbforge file. *)

(** This class takes a file name
   and create an object which represents a dbforge app. The class
   has some methods, in order to be used by Cameleon.*)
class dbforge_app : string ->
  object
    (** The box, to pack it in something. *)
    method box : GPack.box

    (** The method to call to close the app.*)
    method close : unit

    (** The method to call to reload the information from the file.*)
    method reload : unit

    (** The method to call to save the information in the file.*)
    method save : unit

    (** Indicate whether the description has changed,
       i.e. if the file must be saved. *)
    method changed : bool

    (** To perform some initializations with a window. *)
    method init_window : GWindow.window -> unit
  end
