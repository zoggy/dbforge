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

type concrstx =
    RawStr of string
  | IStr of string
  | Var of string
  | Block of concrstx list
  | Valign of concrstx list
  | Halign of concrstx list
  | Indent of concrstx

val sqlquote : string -> string

val pp_concr : ?escape_dblquotes: bool -> int -> (string, string) Hashtbl.t option -> concrstx -> string

val make_pp_query : Sqml_sqlstx.query -> concrstx
val make_pp_ordering : Sqml_sqlstx.ordering -> concrstx
val make_pp_column : Sqml_sqlstx.column -> concrstx
val make_pp_query_exp : Sqml_sqlstx.query_exp -> concrstx
val make_pp_select : Sqml_sqlstx.select -> concrstx
val make_pp_selection : Sqml_sqlstx.selection -> concrstx
val make_pp_from : Sqml_sqlstx.from -> concrstx
val make_pp_groupby : Sqml_sqlstx.group_by -> concrstx
val make_pp_condition : Sqml_sqlstx.where -> concrstx
val make_pp_pred : Sqml_sqlstx.predicate -> concrstx
val make_pp_comp : Sqml_sqlstx.comparison -> concrstx
val make_pp_exp : Sqml_sqlstx.exp -> concrstx
val make_pp_atom : Sqml_sqlstx.atom -> concrstx
val make_pp_functioncall : Sqml_sqlstx.functioncall -> concrstx
val string_of_function_label : Sqml_sqlstx.function_label -> string
val make_pp_parameter : Sqml_sqlstx.parameter -> concrstx

val make_pp_full_select : Sqml_sqlstx.select * Sqml_sqlstx.ordering list -> concrstx
