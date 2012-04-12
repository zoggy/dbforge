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

exception Invalid_input of string

module StringComparable =
struct
  type t = string
  let  compare = (Pervasives.compare : string -> string -> int)
end

module StringMap = Map.Make (StringComparable)

type yes_no_maybe = Yes | No | Maybe

let apply_opt f v =
  match v with
    | None   -> None
    | Some v -> Some (f v)

let join = fun ?(sep = " ") ~to_string list ->
  let result = ref "" and first = ref true in
  let rec join = function
    | []       -> ()
    | hd :: tl ->
        join tl;
        if   !first
        then first  := false
        else result := sep ^ !result;
        result := (to_string hd) ^ !result
  in
    join list; !result

let join_opt = fun ?(sep = " ") list ->
  let result = ref "" and first = ref true in
  let rec join = function
    | []              -> ()
    | None :: tl      -> join tl
    | (Some hd) :: tl ->
        join tl;
        if   !first
        then first  := false
        else result := sep ^ !result;
        result := hd ^ !result
  in
    join list; !result

let uniq = fun ?(sorted = true) list ->
  let list =
    if sorted then list else List.sort (Pervasives.compare) list
  in
    match list with
      | [] -> []
      | hd :: tl ->
          let rec uniq = fun x -> function
            | [] -> [x]
            | hd :: tl when hd = x -> uniq x tl
            | hd :: tl when hd > x -> x :: (uniq hd tl)
            | _ ->
                invalid_arg "list is not sorted"
          in
            uniq hd tl

let unopt = function
  | Some p -> p
  | None   -> failwith "Cannot `unopt' None"

let i_int     = fun (_ : int)  -> ()
let i_bool    = fun (_ : bool) -> ()

let ie = fun () -> failwith "Internal error"

let trim =
  let trim_re1 = Str.regexp "^[[:space:]]+"
  and trim_re2 = Str.regexp "[[:space:]]$" in
    fun s ->
      Str.global_replace trim_re2 ""
        (Str.global_replace trim_re1 "" s)

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
      | _ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      | Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)
