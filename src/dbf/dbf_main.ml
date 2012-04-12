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

open Dbf_sql.SQL_db
open Dbf_sql

type mode = Gencode

let indexes_of_table = fun t ->
  List.filter (fun i -> (table_of_index i) == t)

let indexes_of_vtable = fun t ->
  List.filter
    (fun i ->
       ((table_of_index i) == t.vt_ftable ||
        (List.exists (fun (t, _) -> (table_of_index i) == t) t.vt_join)))

let remove_prefix pref s =
  let lenp = String.length pref in
  let len = String.length s in
  if len <= lenp then
    s
  else
   (if String.lowercase (String.sub s 0 lenp) =
       String.lowercase pref
    then
      String.sub s lenp (len - lenp)
    else
      s
   )

let usage = Printf.sprintf "Usage: %s [options] <file>\n" Sys.argv.(0)

let in_file  = ref None
let out_file = ref None
let mode = ref Gencode

(** Will remove the given prefix from table names to get the module names.*)
let remove_table_prefix = ref None

let options = [
  "--version",
  Arg.Unit (fun () -> print_endline Dbf_installation.software_version; exit 0),
  "\tprint version and exit" ;

  "-o", Arg.String (fun s -> out_file := Some s),
  "file\twrite to file instead of standard output" ;

  "--remove-table-prefix", Arg.String (fun s -> remove_table_prefix := Some s),
  "<prefix>\n\t\tremove this prefix from table names to get (simpler) module names" ;
]

let main () =
  Arg.parse options
    (fun s ->
      match !in_file with
        None -> in_file := Some s;
      | Some f -> failwith usage
    )
    (usage^"where options are:");
  let in_file =
    match !in_file with
      None -> failwith usage
    | Some f -> f
  in
  let out =
    match !out_file with
      None -> stdout
    | Some file -> open_out file
  in
  begin
    match !mode with
      Gencode ->
        let db = Dbf_sql_io.db_of_file in_file in
        (* sort columns by name in tables *)
        let comp_cols c1 c2 =
          Pervasives.compare
            (String.lowercase c1.col_name)
            (String.lowercase c2.col_name)
        in
        let sort_cols_in_table ta =
         ta.ta_columns <- List.sort comp_cols ta.ta_columns
        in
        List.iter sort_cols_in_table db.db_tables;

        flush stdout;
        if List.exists (fun t -> t.ta_logged) db.db_tables then
          begin
            Printf.fprintf out "\nlet log_who : (unit -> Dbf_sql_misc.log_who) ref = ref (fun () -> 0)\n\n";
            output_string out
            "let who_modified_what =
  let rec iter acc current = function
    [] -> List.rev acc
  | [id,d,ac,_] -> List.rev ((id,d,ac,current) :: acc)
  | (id1,d1,ac1,_)::(id2,d2,ac2,t2)::q ->
    iter ((id1,d1,ac1,t2)::acc) current ((id2,d2,ac2,t2)::q)
  in
  let comp (_,(d1:float),_,_) (_,(d2:float),_,_) = compare d1 d2 in
  fun current l -> iter [] current (List.sort comp l)
";
          end;

        List.iter
        (fun table ->
           let idxes = indexes_of_table table db.db_indexes in
           let module_name =
             match !remove_table_prefix with
               None -> String.capitalize table.ta_name
             | Some s ->
                 String.capitalize
                  (remove_prefix s table.ta_name)
           in
           Dbf_sql_gen.print (table, module_name, idxes) out)
        db.db_tables;
        List.iter
        (fun vtable ->
           let idxes = indexes_of_vtable vtable db.db_indexes in
           Dbf_sql_vgen.print (vtable, idxes) out)
        db.db_vtables;
        Printf.fprintf out
        "\nmodule Queries = functor (Sql : Dbf_sql_driver.SqlDriver) -> struct\n";
        List.iter
        (fun query ->
           Dbf_sql_qgen.print query out)
        db.db_queries;
        Printf.fprintf out "end\n"
  end;
  close_out out

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let _ = safe_main main
