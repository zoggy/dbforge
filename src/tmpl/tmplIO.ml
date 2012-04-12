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

open Template
open Format

let print_template = fun chan ->
  let rec print = fun formatter -> function
    | Tmpl_Text text -> begin
        match List.rev (Str.split_delim (Str.regexp "\r?\n") text) with
          | hd :: tl ->
              List.iter
                (fun s -> Format.fprintf formatter
                   "Pervasives.output_string tmpl__channel \"%s\\n\";@ "
                   (String.escaped s))
                (List.rev tl);
              if hd <> "" then
                Format.fprintf formatter
                  "Pervasives.output_string tmpl__channel \"%s\";@ "
                  (String.escaped hd)
          | _ ->
              ()
      end

    | Tmpl_Block (caml, ts) ->
        Format.fprintf formatter
          "@[<2>begin %s@ in@ %a@]@\nend;@\n"
          caml
          (fun formatter ts ->
             List.iter (fun t -> print formatter t) ts)
          ts

    | Tmpl_Iter (symbol, list, ts) ->
        Format.fprintf formatter "@[<2>begin@\n";
        Format.fprintf formatter "@[<2>List.iter@\n";
        Format.fprintf formatter "@[<2>(fun %s -> begin@\n%a@]"
          symbol
          (fun formatter () -> List.iter (fun t -> print formatter t) ts) ();
        Format.fprintf formatter "end)@\n";
        Format.fprintf formatter "%s@]@]@\n" list;
        Format.fprintf formatter "end;@\n"

    | Tmpl_If (caml, ts) ->
        Format.fprintf formatter
          "@[<2>if (%s) then begin@\n%a@\nend;@\n"
          caml
          (fun formatter () -> List.iter (fun t -> print formatter t) ts) ()

    | Tmpl_For (symbol, ocaml, ts) ->
        Format.fprintf formatter "@[<2>begin@\n";
        Format.fprintf formatter "@[<2>let (min, max) = (%s)@ in @\n" ocaml;
        Format.fprintf formatter "for %s = min to max do@\n%a@\ndone;"
          symbol
          (fun formatter () -> List.iter (fun t -> print formatter t) ts) ();
        Format.fprintf formatter "@]@]@\n";
        Format.fprintf formatter "end;@\n"

    | Tmpl_Caml caml ->
        Format.fprintf formatter "@[<2>begin@\n";
        Format.fprintf formatter "@[<2>let string = (%s)@ in@\n" caml;
        Format.fprintf formatter "Pervasives.output_string tmpl__channel string";
        Format.fprintf formatter "@]@]@\n";
        Format.fprintf formatter "end;@\n"
  in
    fun (Tmpl (init, template)) ->
      let formatter = formatter_of_out_channel chan in
        begin match init with
          | None   -> ()
          | Some s -> Format.fprintf formatter "%s@\n@\n" s
        end;
        Format.fprintf formatter
          "@[<2>let print = fun tmpl__env tmpl__channel ->@\n%a@]\n"
          (fun formatter () -> List.iter (print formatter) template) ();
        Format.pp_print_flush formatter

let of_lexbuf = fun lexbuf ->
  let state = ref TmplLexer.LexText in
    try
      TmplLexer.line := 0;
      TmplParser.main (TmplLexer.lex state) lexbuf
    with
      | TmplLexer.Lex_error -> raise Parsing.Parse_error

let of_channel = fun ?(close = true) channel ->
  try
    of_lexbuf (Lexing.from_channel channel)
  with
    | e -> begin
        (try  if close then Pervasives.close_in channel
         with _ -> ());
        raise e
      end

let of_file = fun ~filename ->
  let chan = Pervasives.open_in filename in
    of_channel ~close:true chan
