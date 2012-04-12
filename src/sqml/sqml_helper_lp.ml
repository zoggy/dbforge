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

(* ugly hacks 'cause lexer must be defined before parser *)
open Lexing

class err_report lexbuf = object (self)
  val mutable numline = 1
  val mutable abscharposlastline = 0

  method ack_nl =
    numline <- numline + 1;
    abscharposlastline <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  method numline = numline
  method numchar =
    lexbuf.lex_abs_pos + lexbuf.lex_start_pos - abscharposlastline + 1

  method error : 'a. string -> 'a = fun s ->
    raise (Sqml_sqlstx.Syntax_error (self#numline, self#numchar, s))

end

let err_report = ref (None : err_report option)

let err () =
  match !err_report with
    None -> failwith "should be called after the beginning of a parse ..."
  | Some x -> x

let errstart l =
  match !err_report with
    None -> err_report := Some (new err_report l)
  | _ -> ()
