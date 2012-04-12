module StringComparable =
struct
  type t = string
  let  compare = (Pervasives.compare : string -> string -> int)
end

module StringMap = Map.Make (StringComparable)

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

let join_opt = fun ?(sep = " ") ~to_string list ->
  let result = ref "" and first = ref true in
  let rec join = function
    | []              -> ()
    | None :: tl      -> join tl
    | (Some hd) :: tl ->
        join tl;
        if   !first
        then first  := false
        else result := sep ^ !result;
        result := (to_string hd) ^ !result
  in
    join list; !result

let apply_opt = fun fct value ->
  match value with
    | None   -> None
    | Some v -> Some (fct v)

let unopt = function
  | Some v -> v
  | None   -> failwith "Cannot unopt None"

let ie = fun () -> failwith "Internal error"

(*c==v=[String.no_blanks]=1.0====*)
let no_blanks s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      ' ' | '\n' | '\t' | '\r' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
(*/c==v=[String.no_blanks]=1.0====*)

(** {2 For logged tables} *)

type log_who = int

type log_date = float
type log_action = Insert | Delete | Update
let string_of_action = function
  Insert -> "0"
| Delete -> "1"
| Update -> "2"

let action_of_string = function
  "0" -> Insert
| "1" -> Delete
| _ -> Update

let who_modified_what =
  let rec iter acc current = function
    [] -> List.rev acc
  | [id,d,ac,_] -> List.rev ((id,d,ac,current) :: acc)
  | (id1,d1,ac1,_)::(id2,d2,ac2,t2)::q ->
    iter ((id1,d1,ac1,t2)::acc) current ((id2,d2,ac2,t2)::q)
  in
  let comp (_,(d1:float),_,_) (_,(d2:float),_,_) = compare d1 d2 in
  fun current l -> iter [] current (List.sort comp l)
