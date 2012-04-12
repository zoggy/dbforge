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

(* abstract syntax for pretty printing *)
open Printf

type concrstx =
  | RawStr of string
  | IStr of string (* string with characters \n and \t to be interpreted *)
  | Var of string
  | Block of concrstx list
  | Valign of concrstx list
  | Halign of concrstx list
  | Indent of concrstx

let escape_dbl_quotes s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '"' -> Buffer.add_string b "\\\""
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let pp_concr ?(escape_dblquotes=false) tabsize h c =
  let b = Buffer.create 0 in
  let col = ref 0 in
  let prkspace k = for i = 1 to k do Buffer.add_char b ' ' done; col := !col + k in
  let prspace_till c =
    while !col < c do
      Buffer.add_char b ' ';
      incr col;
    done
  in
  let pristr k s =
    for i = 0 to String.length s - 1 do
      if s.[i] = '\t' then
	prkspace tabsize
      else (
	Buffer.add_char b s.[i];
	incr col;
	if s.[i] = '\n' then ( col := 0; prkspace k );
      )
    done
  in
  let prrawstr ?(force_noescape=false) s =
    let s =
      if escape_dblquotes && not force_noescape then
	escape_dbl_quotes s
      else s
    in
    Buffer.add_string b s; col := !col + String.length s
  in
  let getvar v = match h with None -> ":" ^ v | Some h -> Hashtbl.find h v in
  let rec pr k = function
    | RawStr s -> prrawstr s
    | IStr s -> pristr k s
    | Var i -> prrawstr ~force_noescape:true (getvar i)
    | Block l -> List.iter (fun c -> pr k c; pristr k "\n") l
    | Halign l ->
	ignore (List.fold_left (fun isfirst c -> if not isfirst then prkspace 1; pr !col c; false) true l)
    | Valign l ->
	List.iter (fun c -> pr k c; pristr k "\n") l
    | Indent c -> prspace_till (k + tabsize); pr (k + tabsize) c
  in
  pr 0 c;
  Buffer.contents b

let list_between f x l =
  snd (List.fold_right (fun e (islast, acc)-> (false, f e :: (if not islast then x :: acc else acc))) l (true, []))

let r x = RawStr x

let sqlquote s =
  let b = Buffer.create 0 in
  Buffer.add_char b '\'';
  for i = 0 to String.length s - 1 do
    Buffer.add_char b s.[i];
    if s.[i] = '\'' then
      Buffer.add_char b '\'';
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

let rec make_pp_query (e, ol) =
  Valign ((make_pp_query_exp e)::
	    if ol = [] then []
	    else [Halign (r "ORDER BY"
			 :: (list_between make_pp_ordering (r ",") ol))])
and make_pp_ordering (c, tri) =
  Halign  ((match c with `numcolumn i -> r (string_of_int i) | `column c -> make_pp_column c)
	  :: (match tri with None -> [] | Some `asc -> [r "ASC"] | Some `desc -> [r "DESC"]))
and make_pp_column = function
    `ref s -> r s
  | `refdotref (s1, s2) -> r (sprintf "%s.%s" s1 s2)
and make_pp_query_exp = function
    `select select -> make_pp_select select
  | `union (qe1, qe2) -> Valign [ make_pp_query_exp qe1 ; r "UNION" ; make_pp_query_exp qe2 ]
  | `unionall (qe1, qe2) -> Valign [ make_pp_query_exp qe1 ; r "UNION ALL" ; make_pp_query_exp qe2 ]
and make_pp_select ((md, sel, from, where, grp, hav):Sqml_sqlstx.select) =
  Block (List.flatten [
	   [ Halign (List.flatten [
		       [r "SELECT"];
		       (match md with `nomod -> [] | `all -> [r "ALL"] | `distinct -> [r "DISTINCT"]);
		       [make_pp_selection sel; r "FROM"; make_pp_from from]]) ];
	 (match where with None -> [] | Some cond -> [Halign [r "WHERE"; Indent (make_pp_condition cond)]]);
	 (match grp with None -> [] | Some groupby -> [Halign [r "GROUP BY"; make_pp_groupby groupby]]);
	 (match hav with None -> [] | Some cond -> [Halign [r "HAVING"; make_pp_condition cond]])
	 ])
and make_pp_selection = function
    `star -> r "*"
  | `list l -> Halign (list_between make_pp_exp (r ",") l)
and make_pp_from l =
  Halign (list_between (function
			    `table t -> r t
			  | `tableas (t, a) -> Halign [r t; r "AS"; r a]) (r ",") l)
and make_pp_groupby l =
  Halign (list_between make_pp_column (r ",") l)
and make_pp_condition = function
    `cand (c1, c2) -> Halign [r "("; make_pp_condition c1; r "AND"; make_pp_condition c2; r ")"]
  | `cor (c1, c2) -> Halign [r "("; Valign [Indent (make_pp_condition c1);
					    Halign [r "OR"; Indent (make_pp_condition c2)]]
			    ; r ")"]
  | `cnot c -> Halign [r "NOT"; make_pp_condition c]
  | `p p -> make_pp_pred p
and make_pp_pred = function
    `comparisonexp (exp1 , comparison , exp2) -> Halign [make_pp_exp exp1; make_pp_comp comparison; make_pp_exp exp2]
  | `comparisonselect (exp , comparison , select )-> Halign [make_pp_exp exp; make_pp_comp comparison; r "("; make_pp_select select; r ")"]
  | `between (bool , exp1 , exp2 , exp3) ->
      Halign [make_pp_exp exp1; if bool then r "NOT BETWEEN" else r "BETWEEN"; make_pp_exp exp2; r "AND"; make_pp_exp exp3]
  | `like (bool , exp , atom , atopt) ->
      Halign [make_pp_exp exp; if bool then r "NOT LIKE" else r "LIKE"; make_pp_atom atom;
	      (match atopt with None -> r "" | Some atom -> Halign [r "ESCAPE"; make_pp_atom atom])]
  | `iscolnull (bool , column) ->
      Halign [make_pp_column column; if bool then r "IS NOT NULL" else r "IS NULL"]
  | `in_select (bool , exp , select) ->
      Halign [make_pp_exp exp; if bool then r "NOT IN" else r "IN"; make_pp_select select]
  | `in_atom_list (bool , exp , atl) ->
      Halign [make_pp_exp exp; r "IN"; r "("; Halign (list_between make_pp_atom (r ",") atl); r ")"]
  | `allorany (exp , comparison , allsome , select) ->
      Halign [make_pp_exp exp; make_pp_comp comparison;
	      (match allsome with `all -> r "ALL" | `some -> r "SOME");
	      make_pp_select select]
  | `exists select -> Halign [ r "EXISTS"; make_pp_select select]
and make_pp_comp = function
  | `eq -> r "="
  | `neq -> r "<>"
  | `lt -> r "<"
  | `gt -> r ">"
  | `lte -> r "<="
  | `gte -> r ">="
and make_pp_exp = function
    `binop (pmtd , exp1 , exp2) ->
      Halign [r "("; make_pp_exp exp1;
	      (match pmtd with
		 `plus -> r "+"
	       | `minus -> r "-"
	       | `times -> r "*"
	       | `div -> r "/");
	      make_pp_exp exp2; r ")"]
  | `uminus exp -> Halign [r "-"; make_pp_exp exp]
  | `atom atom -> make_pp_atom atom
  | `column column -> make_pp_column column
  | `functioncall functioncall -> make_pp_functioncall functioncall
and make_pp_atom = function
    `parameter parameter -> make_pp_parameter parameter
  | `string string -> r (sqlquote string)
  | `int int -> r (string_of_int int)
  | `inttoomuch string -> r string
  | `float (int1 , int2 , float) -> r (sprintf "%*.*f" int1 int2 float)
  | `floattoomuch string -> r string
  | `user -> r "USER"
and make_pp_functioncall (function_label , x) =
	Halign [ r ((string_of_function_label function_label)^"(");
		 (match x with
		    `star -> r " * )"
		  | `distinct column -> Halign [ r "DISTINCT"; make_pp_column column; r ")" ]
		  | `exp (`nomod , exp) -> Halign [  make_pp_exp exp ; r ")" ]
		  | `exp (`all, exp) -> Halign [  r "ALL"; make_pp_exp exp ; r ")" ]
		 );]
and string_of_function_label = function
    `min -> "MIN"
  | `max -> "MAX"
  | `sum -> "SUM"
  | `count -> "COUNT"
  | `avg -> "AVG"
  | `other string -> string
and make_pp_parameter = function
    `single string -> Var string
  | `single_annotated (string, _, _) -> Var string
  | `couple (string , string2) -> failwith "I don't know what double parameters mean"
  | `indicator (string , string2) -> failwith "I don't know what indicator parameters mean"


let make_pp_full_select ((s:Sqml_sqlstx.select), (ol:Sqml_sqlstx.ordering list)) =
  Block (List.flatten [
	   [make_pp_select s];
	   (if ol = [] then [] else
	      [Halign (r "ORDER BY"
		       :: (list_between make_pp_ordering (r ",") ol))]);
	   [r ";"]
	 ])
