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
  open TmplParser

  type symbol = string
  type state  = LexText | LexExpression

  exception Eof
  exception Lex_error

  let string_of_char = fun c -> String.make 1 c


  let line = ref 1
  let incr_line_from_string s =
    for i = 0 to String.length s - 1 do
      match s.[i] with
        '\n' -> incr line
      | _ -> ()
    done
}

let alpha    = ['a'-'z''A'-'Z']
let digit    = ['0'-'9']
let xdigit   = ['0'-'9''A'-'F''a'-'f']
let alphanum = ['a'-'z''A'-'Z''0'-'9']
let ident    = alpha alphanum*
let int      = digit+
let space    = [' ''\t''\r']
let newline  = ['\n']

let octa_char = '\\'  digit digit
let hexa_char = "\\x" xdigit xdigit xdigit
let esc_char  = ('\\' _) | octa_char | hexa_char

rule lex state = parse
  | "" {
      match !state with
        | LexText       -> lex_tmpl       state lexbuf
        | LexExpression -> lex_expression state lexbuf
    }
  | eof { TokE_EOF }

and lex_tmpl state = parse
  | "<!"                  { state := LexExpression; TokE_ExprBegin       }
  | "<?"                  { state := LexExpression; TokE_BlockBegin      }
  | "<#"                  { state := LexExpression; TokE_InitBegin       }
  | "<?/"                 { state := LexExpression; TokE_CloseBlockBegin }
  | [^'<' '\\']+ as s     { incr_line_from_string s ; TokE_Text s     }
  | "\\<"                 { TokE_Text "<"   }
  | _ as c                { TokE_Text (string_of_char c) }
  | eof                   { TokE_EOF        }

and lex_expression state = parse
  | space+                { lex_expression state lexbuf }
  | newline               { incr line; lex_expression state lexbuf }
  | ident as id           { TokE_Id id                  }

  (* tags *)
  | "!>"                  { state := LexText; TokE_ExprEnd  }
  | "?>"                  { state := LexText; TokE_BlockEnd }
  | "#>"                  { state := LexText; TokE_InitEnd  }

  (* Ponctuation *)
  | ","                   { TokE_Comma     }
  | ":"                   { TokE_Colon     }
  | "="                   { TokE_Eq        }

  | "["
    { let buffer = Buffer.create 256 in
    lex_caml_block buffer lexbuf;
    TokE_Caml (Buffer.contents buffer) }

  (* EOF and garbage *)
  | _                     { raise Lex_error }
  | eof                   { TokE_EOF        }

and lex_caml_block buffer = parse
  | [^']' '\\' '"' '(' '\'']+ as s
    { incr_line_from_string s ;
      Buffer.add_string buffer s;
      lex_caml_block buffer lexbuf }

  | ']'  { () }

  | "\\]"
      { Buffer.add_char buffer ']';
        lex_caml_block buffer lexbuf }

  | ("'" (esc_char | [^'\\' '\'']) "'") as s
    { Buffer.add_string buffer s;
      lex_caml_block buffer lexbuf }

  | ('"' (esc_char | [^'\\' '"'])* '"') as s
    { Buffer.add_string buffer s;
      lex_caml_block buffer lexbuf }

  | "(*"
    { Buffer.add_string buffer "(*";
      lex_caml_comment buffer 0 lexbuf;
      Buffer.add_string buffer "*)";
    lex_caml_block buffer lexbuf }

  | _ as c
    { Buffer.add_char buffer c;
      lex_caml_block buffer lexbuf }

  | eof
  { raise Lex_error }

and lex_caml_comment buffer depth = parse
  | [^'(' '*']+ as s
    { incr_line_from_string s ;
      Buffer.add_string buffer s;
      lex_caml_comment buffer depth lexbuf }

  | "(*"
    { Buffer.add_string buffer "(*";
      lex_caml_comment buffer (depth + 1) lexbuf;
      Buffer.add_string buffer "*)"
    }

  | "*)"
    { if   depth = 0
      then ()
      else lex_caml_comment buffer (depth - 1) lexbuf }

  | _ as c
    { Buffer.add_char buffer c;
      lex_caml_comment buffer depth lexbuf }

  | eof
    { raise Lex_error }
