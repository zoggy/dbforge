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

let usage = Printf.sprintf
    "Usage: %s [options] <file>\n" Sys.argv.(0)

let file = ref None
let options = [
  "--version",
  Arg.Unit (fun () -> print_endline Dbf_installation.software_version; exit 0),
  "\tprint version and exit" ;
]

let _ =
  Arg.parse options
    (fun s ->
      match !file with
        None -> file := Some s;
      | Some f -> failwith usage
    )
    (usage^"where options are:");
  ignore(GMain.Main.init ());
  Glade.init ();

  let gui = new Dbf_gtk.gui () in
  (
   match !file with
     None -> ()
   | Some f -> gui#open_file f
  );
  GMain.main ()
