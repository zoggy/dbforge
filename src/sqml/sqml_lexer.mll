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

{
 open Sqml_helper_lp
 open Sqml_parser

   (* keywords for faster lex gen *)
 let keywords = Hashtbl.create 0

 let _ =
   List.iter (fun (kw, s) -> Hashtbl.add keywords s kw)
     [
 (ADA, "ADA");  (ALL, "ALL");  (AND, "AND");
 (ANY, "ANY");  (AS, "AS");  (ASC, "ASC");  (AUTHORIZATION, "AUTHORIZATION");
 (BETWEEN, "BETWEEN");  (BY, "BY");  (C, "C");  (CHECK, "CHECK");
 (CLOSE, "CLOSE");  (COBOL, "COBOL");  (COMMIT, "COMMIT");  (CONTINUE, "CONTINUE");
 (CREATE, "CREATE");  (CURRENT, "CURRENT");  (CURSOR, "CURSOR");  (DECIMAL, "DECIMAL");
 (DECLARE, "DECLARE");  (DEFAULT, "DEFAULT");  (DELETE, "DELETE");  (DESC, "DESC");
 (DISTINCT, "DISTINCT");  (DOUBLE, "DOUBLE");  (ESCAPE, "ESCAPE");  (EXISTS, "EXISTS");
 (FETCH, "FETCH");  (FLOAT, "FLOAT");  (FOR, "FOR");  (FOREIGN, "FOREIGN");
 (FORTRAN, "FORTRAN");  (FOUND, "FOUND");  (FROM, "FROM");  (GRANT, "GRANT");
 (GROUP, "GROUP");  (HAVING, "HAVING");  (IN, "IN");  (INDICATOR, "INDICATOR");  (INSERT, "INSERT");
 (INTO, "INTO");  (IS, "IS");  (KEY, "KEY");  (LANGUAGE, "LANGUAGE");  (LIKE, "LIKE");
 (MODULE, "MODULE");  (NOT, "NOT");  (NULL, "NULL");  (NUMERIC, "NUMERIC");  (OF, "OF");
 (ON, "ON");  (OPEN, "OPEN");  (OPTION, "OPTION");  (OR, "OR");  (ORDER, "ORDER"); (OCAML, "OCAML");
 (PASCAL, "PASCAL");  (PLI, "PLI");  (PRECISION, "PRECISION");  (PRIMARY, "PRIMARY");
 (PRIVILEGES, "PRIVILEGES");  (PROCEDURE, "PROCEDURE");  (PUBLIC, "PUBLIC");  (REAL, "REAL");
 (REFERENCES, "REFERENCES");  (ROLLBACK, "ROLLBACK");  (SCHEMA, "SCHEMA");  (SELECT, "SELECT");
 (SET, "SET");  (SMALLINT, "SMALLINT");  (SOME, "SOME");  (SQLCODE, "SQLCODE");
 (TABLE, "TABLE");  (TO, "TO");  (UNION, "UNION");  (UNIQUE, "UNIQUE");
 (UPDATE, "UPDATE");  (USER, "USER");  (VALUES, "VALUES");  (VIEW, "VIEW");
 (WHENEVER, "WHENEVER");  (WHERE, "WHERE");  (WITH, "WITH");  (WORK, "WORK");
 (* special ones *)
 (CHARACTER, "CHAR"); (CHARACTER, "CHARACTER"); (INTEGER, "INT"); (INTEGER, "INTEGER");
 (* known functions *)
 (AVG, "AVG"); (MIN, "MIN");  (MAX, "MAX");  (SUM, "SUM");  (COUNT, "COUNT");
     ]

 (* because there are no parameters with lexical rules ... *)
 let b = ref (Buffer.create 32)
}



rule token = parse
  (* identificators and keywords*)

  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
                        { let s = Lexing.lexeme lexbuf in
			  try
			    Hashtbl.find keywords (String.uppercase s)
			  with Not_found ->
			    IDENT s }

  | "GO"[' ' '\t']*"TO" { GOTO }

  (* comparison operators *)
  | "="	                { COMPARISON `eq }
  | "<>" 		        { COMPARISON `neq }
  | "<"	                { COMPARISON `lt }
  | ">"	                { COMPARISON `gt }
  | "<="	                { COMPARISON `lte }
  | ">="		        { COMPARISON `gte }

  (* arithmetic operators *)
  | "-"                   { MINUS }
  | "+"                   { PLUS }
  | "*"                   { TIMES }
  | "/"                   { DIV }

  (* punctuation *)
  | ":"                   { COLON }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | ","                   { COMMA }
  | "."                   { DOT }
  | ";"                   { SEMICOLON }

  (* numbers *)

  | ['0'-'9']+	        { INTNUM (`inttoomuch (Lexing.lexeme lexbuf)) }
  | ['0'-'9']+"."['0'-'9']*
  | "."['0'-'9']*	        { FLOATNUM (`floattoomuch (Lexing.lexeme lexbuf)) }

  | ['0'-'9']+['e' 'E']['+' '-']?['0'-'9']+
  | ['0'-'9']*"."['0'-'9']*['e' 'E']['+' '-']?['0'-'9']+
    { FLOATNUM (`floattoomuch (Lexing.lexeme lexbuf)) }

  (* strings *)

  | '\''                     { sqlstring lexbuf;
			       let s = Buffer.contents !b in
			       Buffer.reset !b;
			       STRING s }

  (* for error reporting *)
  | '\n'		        { (err ()) # ack_nl ; token lexbuf }

  (* whitespace and comments *)
  | [' ' '\t' '\r']+
  | "--" [^ '\n']*	{ token lexbuf }

  (* eof *)
  | eof                 { EOF }

  | _                   { (err ())#error "lexical error" }

and sqlstring = parse
  | [^ '\''  '\n']+ { Buffer.add_string !b (Lexing.lexeme lexbuf); sqlstring lexbuf }
  | "''"      { Buffer.add_char !b '\''; sqlstring lexbuf}
  | '\n'      { (err ())#ack_nl; Buffer.add_char !b '\n'; sqlstring lexbuf}
  | "'"       { () }

{
  let token l =
    errstart l;
    token l
}
