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

(** Parsing SQL [select] and [union] queries. *)

(** {2 Abstract syntax}*)

(** {3 Abstract Syntax for Queries} *)

type query = query_exp * ordering list
and query_exp = [ `select of select
	    | `union of query_exp * query_exp
	    | `unionall of query_exp * query_exp
	    ]
and select = [ `nomod | `all | `distinct ] * selection * from * where option * group_by option * having option
and selection = [ `star | `list of exp list ]
and from = [ `table of string | `tableas of string * string ] list
and where = condition
and group_by = column list
and column = [ `ref of string | `refdotref of string * string ]
and having = condition
and condition = [ `cand of condition * condition
		| `cor of condition * condition
		| `cnot of condition
		| `p of predicate
		]
and predicate = [ `comparisonexp of exp * comparison * exp
		| `comparisonselect of exp * comparison * select
		| `between of bool * exp * exp * exp (* bool XOR (e1 BETWEEN e2 AND e3) *)
		| `like of bool * exp * atom * atom option (* bool XOR (exp LIKE a1 [ESCAPE a2]) *)
		| `iscolnull of bool * column (* bool XOR (column IS NULL) *)
		| `in_select of bool * exp * select (* bool XOR (exp IN select) *)
		| `in_atom_list of bool * exp * atom list
		| `allorany of exp * comparison * [`all | `some] * select (* exp comparison (ALL | ANY) select *)
		| `exists of select
		]
and comparison = [`eq | `neq | `lt | `gt | `lte | `gte ]
and exp = [ `binop of [ `plus | `minus | `times | `div ] * exp * exp
	  | `uminus of exp
	  | `atom of atom
	  | `column of column
	  | `functioncall of functioncall
	  ]
and atom = [ `parameter of parameter
	   | `string of string
	   | `int of int
	   | `inttoomuch of string
	   | `float of int * int * float
	   | `floattoomuch of string
	   | `user
	   ]
and functioncall = function_label * [ `star | `distinct of column | `exp of [`nomod | `all] * exp]
and function_label = [ `min
		     | `max
		     | `sum
		     | `count
		     | `avg
		     | `other of string
		     ]
and parameter = [ `single of string
		| `couple of string * string
		| `indicator of string * string
		]


(** {3 Abstract Syntax for Insert} *)

and insert = string * (string list)(*columns*) * [`values of [`atom of atom | `null] list | `select of select]

(** {3 Abstract Syntax for Schemas} *)

and cmd = [ `tabledef of tabledef
	  | `viewdef of viewdef
	  | `privdef of privdef
	  | `schemadef of schemadef
	  | `moduledef of moddef
	  | `manip of manipulative_statement
	  | `when_not_found of when_action
	  | `whenever_sqlerror of when_action
	  ]
and schemadef = string (*user*) * schema_element list
and schema_element = [ `tabledef of tabledef | `viewdef of viewdef | `privdef of privdef ]

(** {3 Abstract Syntax for create table} *)

and tabledef = string (*table name*) * table_element list
and table_element = [ `columndef of columndef | `tblcnstr of tblcnstr ]
and columndef = string (*column*) * data_type * column_opt list
and data_type = [ `char of int option
		| `numeric of [ `default | `length of int | `lengthdec of int * int ]
		| `decimal of [ `default | `length of int | `lengthdec of int * int ]
		| `int
		| `smallint
		| `float of int option
		| `real
		| `doubleprecision ]
and literal = [	`string of string
	      | `int of int
	      | `inttoomuch of string ]
and column_opt = [ `not_null
		 | `not_null_unique
		 | `not_null_primary_key
		 | `default of literal
		 | `default_null
		 | `default_user
		 | `check of condition
		 | `references of string(*table*) * string list(*columns*)
		 ]
and tblcnstr = [ `unique of string list (*columns*)
	       | `primkey of string list (*columns*)
	       | `foreignkey of string list (*columns*) * string (*table*) * string list (*columns of table*)
	       | `check of condition
	       ]
(** {3 Abstract Syntax for create view} *)

and viewdef = string(*view name*) * string list (*columns*) * select * [`check | `nocheck]

(** {3 Abstract Syntax for grant} *)

and privdef = privilege * string(*table*) * grantee list * [ `nograntoption | `grantoption ]
and privilege = [ `all | `some of operation list ]
and operation = [ `select
		| `insert
		| `delete
		| `update of string list (*columns*)
		| `references of string list (*columns*)
		]
and grantee = [ `public | `user of string ]

(** {3 Abstract Syntax for modules} *)

and moddef = string option (*module*) * lang * string (*user*) * cursor_def list * procedure_def list
and lang = [ `cobol | `fortran | `pascal | `pli | `c | `ada | `ocaml]
and cursor_def = string (*cursor*) * query_exp * ordering list
and ordering = [ `numcolumn of int | `column of column ] * [ `asc | `desc] option
and procedure_def = string (*procedure*) * parameter_def list * manipulative_statement list
and parameter_def = [ `par of string (*parameter*) * data_type | `sqlcode ]
and manipulative_statement = [ `close of string
			     | `commit
			     | `delete_pos of string (*table*) * string (*cursor*)
			     | `delete_where of string (*table*) * condition option
			     | `insert of insert
			     | `fetch of string (*cursor*) * parameter list (*parameters*)
			     | `opencursor of string (*cursor*)
			     | `rollback
			     | `update_pos of string (*table*) * assignment list * string (*cursor*)
			     | `update_where of string (*table*) * assignment list * condition option
			     | `select of select * parameter list(*targets*)
			     ]
and assignment = [ `column_exp of string (*column*) * exp | `column_null of string ]
and when_action = [ `goto of string | `continue]

(** {2 Parsing} *)

exception Syntax_error = Sqml_sqlstx.Syntax_error

let parse_full_select lexbuf =
  Sqml_parser.full_select Sqml_lexer.token lexbuf

let parse_query lexbuf =
  Sqml_parser.query Sqml_lexer.token lexbuf

let parse_command lexbuf =
  Sqml_parser.cmd Sqml_lexer.token lexbuf

let parse_command_list lexbuf =
  Sqml_parser.sql_cmd_list Sqml_lexer.token lexbuf

let query_of_string s =
  Sqml_parser.query Sqml_lexer.token (Lexing.from_string s)
