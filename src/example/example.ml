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

(*c==v=[String.string_of_date]=1.0====*)
let string_of_date =
  let raw_string_of_date ~f_mon ~f_wday ~wday ~hours ~secs d =
    let tm = Unix.localtime d in
    let mon = f_mon tm.Unix.tm_mon in
    let day = f_wday tm.Unix.tm_wday in
    Printf.sprintf
      "%s%02d-%s-%4d%s"
      (if wday then day^", " else "")
      tm.Unix.tm_mday mon (tm.Unix.tm_year + 1900)
      (
       if hours then
         Printf.sprintf  " %02d:%02d%s"
           tm.Unix.tm_hour tm.Unix.tm_min
           (if secs then Printf.sprintf ":%02d" tm.Unix.tm_sec else "")
       else
         ""
      )
  in
  let mon_of_int_fr = function
      0 -> "Jan"
    | 1 -> "Fev"
    | 2 -> "Mar"
    | 3 -> "Avr"
    | 4 -> "Mai"
    | 5 -> "Jui"
    | 6 -> "Jul"
    | 7 -> "Aou"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | _ -> "Dec"
  in
  let wday_of_int_fr = function
      0 -> "Dimanche"
    | 1 -> "Lundi"
    | 2 -> "Mardi"
    | 3 -> "Mercredi"
    | 4 -> "Jeudi"
    | 5 -> "Vendredi"
    | _ -> "Samedi"
  in
  let mon_of_int_en = function
      0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | _ -> "Dec"
  in
  let wday_of_int_en = function
      0 -> "Sunday"
    | 1 -> "Monday"
    | 2 -> "Tuesday"
    | 3 -> "Wednesday"
    | 4 -> "Thursday"
    | 5 -> "Friday"
    | _ -> "Saturday"
  in
  fun ?(fr=false)
      ?(wday=false) ?(hours=true) ?(secs=false) d ->
    let (f_mon, f_wday) =
      if fr then
	(mon_of_int_fr, wday_of_int_fr)
      else
	(mon_of_int_en, wday_of_int_en)
    in
    raw_string_of_date ~f_mon ~f_wday
      ~wday ~hours ~secs d
(*/c==v=[String.string_of_date]=1.0====*)


let _ = Dbf_mysql.debug_printer :=
  (fun s -> print_endline (Printf.sprintf "\027[36;1m%s\027[0m" s))


module D = Dbf_mysql.MysqlDriver

let dbname = ref "test"
let user = ref "guesdon"

let db = D.connect ~user: !user ~database: !dbname ();;

prerr_endline (Printf.sprintf "Connected to %s as %s" !dbname !user);;

module People = Base.People (D)
module Queries = Base.Queries (D)
let print_t t =
  let s = Printf.sprintf "\027[32;1m{id=%d ; name=%s ; text=%s}\027[0m"
      t.People.id t.People.name
      (match t.People.text with
        None -> "NULL"
      | Some s -> "Some "^s
      )
  in
  print_endline s

let message s =
  let s = Printf.sprintf "\027[42;1m%s\027[0m" s in
  print_endline s

let main () =
  message "Creating table people...";
  People.create db;

  message "Inserting some stuff..." ;
  let l =
    [ "Martin", None ;
      "Durand", Some "Monsieur Durand bidule" ;
      "Dupont", Some "A ne pas confondre avec Dupond" ;
    ]
  in
  List.iter
    (fun (name,text) -> People.insert db ~name ~text ())
    l;

  message "Reading contents of table..." ;
  let l = People.select db () in
  List.iter print_t l;

  message "Using get_with_name_like custom query...";
  let l = Queries.get_with_name_like db ~name: "D%" in
  List.iter
    (fun (name,id) -> Printf.printf "name=%s,id=%d" name id; print_newline())
    l;

  message "Reading Martin..." ;
  let l = People.select db ~name: "Martin" () in
  List.iter print_t l;

  message "Reading where text is null..." ;
  let l = People.select db ~text: None () in
  List.iter print_t l;

  message "Setting text of Dupont to None";
  People.update db ~key_name: "Dupont" ~text: None ();

  message "Reading where text is null..." ;
  let l = People.select db ~text: None () in
  List.iter print_t l;

  message "Searching id=1";
  let t = People.PKey.search db ~id: 1 in
  begin
    match t with
      None -> assert false
    | Some t -> print_t t
  end;

  message "Deleting Dupont";
  People.delete db ~name: "Dupont" ();

  message "Modifying vairous times the comment of Martin";
  People.update db ~key_name: "Martin" ~text: (Some "Comment 1") ();
  People.update db ~key_name: "Martin" ~text: (Some "Comment 2") ();
  People.update db ~key_name: "Martin" ~text: (Some "Comment 3") ();
  People.update db ~key_name: "Martin" ~text: (Some "Comment 4") ();


  message "Reading contents of table..." ;
  let l = People.select db () in
  List.iter print_t l;

  message "Reading log table...";
  let flog t =
    print_endline "Log of "; print_t t;
    let l = Dbf_sql_misc.who_modified_what t (People.select_log db ~id: t.People.id ()) in
    let f (who,date,action,t) =
      Printf.printf "%d,%s,%s:" who (string_of_date date)
        (match action with
          Dbf_sql_misc.Insert -> "Insert"
        | Dbf_sql_misc.Update -> "Update"
        | Dbf_sql_misc.Delete -> "Delete"
        );
      print_t t
    in
    List.iter f l
  in
  List.iter flog l;

  message "Raw log:";
  let l = People.select_log db () in
  let f (who,date,action,t) =
    Printf.printf "%d,%s,%s:" who (string_of_date date)
      (match action with
        Dbf_sql_misc.Insert -> "Insert"
        | Dbf_sql_misc.Update -> "Update"
        | Dbf_sql_misc.Delete -> "Delete"
      );
    print_t t
  in
  List.iter f l;

  message "Dropping table people...";
  People.drop db;

  message "Disconnecting...";
  D.disconnect db;

  message "Done. Bye."

let _ = main ()
