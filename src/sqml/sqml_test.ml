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

open Printf
open Sqml_pp

let end_of_lex l =
  try Sqml_parser.ugly_eof (fun _ -> failwith "ugly") l; true
  with _ -> false

let _ =
  if not !Sys.interactive then
  try
    let lexbuf = Lexing.from_channel stdin in
    while not (end_of_lex lexbuf) do
      let result = Sqml_parser.full_select Sqml_lexer.token lexbuf in
      let p = make_pp_full_select result in
      printf "%s\n" (pp_concr 2 None p);
      flush stdout
    done
  with e ->
    printf "%s\n" (Printexc.to_string e)
