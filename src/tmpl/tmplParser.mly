/*********************************************************************************/
/*                Cameleon                                                       */
/*                                                                               */
/*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Library General Public License as            */
/*    published by the Free Software Foundation; either version 2 of the         */
/*    License, or any later version.                                             */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU Library General Public          */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

%{
  module T = Template

  type block_description =
      { bd_id    : string;
        bd_args  : (string * string) list;
        bd_ocaml : string option }

  exception Invalid_template

  let get_block  = fun ts -> function
    | { bd_id    = "block";
        bd_args  = [];
        bd_ocaml = Some ocaml; } ->
        T.Tmpl_Block (ocaml, ts)
    | { bd_id    = "iter";
        bd_args  = ["name", name];
        bd_ocaml = Some ocaml; } ->
        T.Tmpl_Iter (name, ocaml, ts)
    | { bd_id    = "for";
        bd_args  = ["name", name];
        bd_ocaml = Some ocaml; } ->
        T.Tmpl_For (name, ocaml, ts)
    | { bd_id    = "if";
        bd_args  = [];
        bd_ocaml = Some ocaml; } ->
        T.Tmpl_If (ocaml, ts)
    | _ ->
        raise Invalid_template
%}

%token TokE_EOF

%token TokE_Comma
%token TokE_Colon
%token TokE_Eq
%token TokE_Slash
%token TokE_LBracket
%token TokE_RBracket

%token <string> TokE_Id
%token <string> TokE_Caml

%token TokE_InitBegin
%token TokE_InitEnd
%token TokE_ExprBegin
%token TokE_ExprEnd
%token TokE_BlockBegin
%token TokE_CloseBlockBegin
%token TokE_BlockEnd

%token <string> TokE_Text

%start main
%type  <Template.template> main

%%
main:
| chunk_list_0 TokE_EOF      { T.Tmpl (None, List.rev $1)    }
| init chunk_list_0 TokE_EOF { T.Tmpl (Some $1, List.rev $2) }

init:
| TokE_InitBegin TokE_Caml TokE_InitEnd
  { $2 }
;

chunk:
| text
  { T.Tmpl_Text $1 }
| TokE_ExprBegin TokE_Caml TokE_ExprEnd
  { T.Tmpl_Caml $2 }
| block_begin chunk_list_0 block_end
  { if $1.bd_id <> $3 then
      raise Invalid_template;
    get_block (List.rev $2) $1 }
;

text:
| TokE_Text      { $1      }
| TokE_Text text { $1 ^ $2 }
;

block_begin:
| TokE_BlockBegin TokE_Id TokE_BlockEnd
  { { bd_id    = $2;
      bd_args  = [];
      bd_ocaml = None; } }

| TokE_BlockBegin TokE_Id TokE_Caml TokE_BlockEnd
  { { bd_id    = $2;
      bd_args  = [];
      bd_ocaml = Some $3; } }

| TokE_BlockBegin TokE_Id TokE_Colon args_list_1 TokE_BlockEnd
  { { bd_id    = $2;
      bd_args  = $4;
      bd_ocaml = None; } }

| TokE_BlockBegin TokE_Id TokE_Colon args_list_1 TokE_Caml TokE_BlockEnd
  { { bd_id    = $2;
      bd_args  = $4;
      bd_ocaml = Some $5; } }
;

block_end:
| TokE_CloseBlockBegin TokE_Id TokE_BlockEnd
  { $2 }

chunk_list_1:
| chunk              { [$1]     }
| chunk_list_1 chunk { $2 :: $1 }
;

chunk_list_0:
| /* Empty */        { [] }
| chunk_list_1       { $1 }
;

args_list_1:
| arg                        { [$1]     }
| args_list_1 TokE_Colon arg { $3 :: $1 }
;

arg:
| TokE_Id TokE_Eq TokE_Id { ($1, $3) }
;
