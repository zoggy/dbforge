(* $Id: dbf_sql_driver.ml 142 2006-01-12 16:39:05Z zoggy $ *)

module StringComparable =
struct
  type t = string
  let  compare = (Pervasives.compare : string -> string -> int)
end

module StringMap = Map.Make (StringComparable)

module Misc =
struct
  let unopt = function
    | None   -> failwith "Cannot unopt None"
    | Some v -> v

  module Array =
  struct
    let fold_left2 = fun f init a1 a2 ->
      if Array.length a1 <> Array.length a2 then
        invalid_arg "Misc.Array.fold_left2: arrays don't have same size";
      let result = ref init in
        for i = 0 to (Array.length a1) - 1 do
          result := f !result a1.(i) a2.(i)
        done;
        !result

    let combine = fun a1 a2 ->
      if Array.length a1 <> Array.length a2 then
        invalid_arg "Misc.Array.combine: arrays don't have same size";
      Array.init (Array.length a1) (fun i -> (a1.(i), a2.(i)))
  end
end

exception Sql_error of string
exception Invalid_conversion

module type SqlDriver =
sig
  type db
  type cursor

  type result =
    | R_Ok
    | R_Empty
    | R_Fetch of cursor

  type fetch_mode =
    | FM_Array
    | FM_Assoc
    | FM_ListAssoc

  type fetch_result =
    | FR_Array     of string option array
    | FR_Assoc     of string option StringMap.t
    | FR_ListAssoc of (string * string option) array

  type transaction =
    | T_ReadCommited
    | T_Serializable

  type sql_value = string

  val db_id : string

  val escape_identifier : string -> string
  val escape_value      : string -> string

  val connect :
    ?host:string     ->
    ?database:string ->
    ?port:int        ->
    ?password:string ->
    ?user:string     ->
    unit             ->
    db

  val disconnect : db -> unit

  val fetch_row  : ?fm:fetch_mode -> cursor -> fetch_result option
  val fetch_all  : ?fm:fetch_mode -> cursor -> fetch_result list
  val map        : ?fm:fetch_mode -> f:(fetch_result -> 'a) -> cursor -> 'a list

  val begin_transaction  : db -> transaction -> unit
  val end_transaction    : db -> unit
  val cancel_transaction : db -> unit

  val names  : cursor -> string array
  val fields : cursor -> int

  val exec : db -> query:string -> result

  val set_autocommit : db -> autocommit:bool -> unit

  val str2sql   : string -> sql_value
  val bool2sql  : bool   -> sql_value
  val int2sql   : int    -> sql_value
  val int322sql : int32  -> sql_value
  val int642sql : int64  -> sql_value
  val float2sql : float  -> sql_value

  val sql2str   : sql_value -> string
  val sql2bool  : sql_value -> bool
  val sql2int   : sql_value -> int
  val sql2int32 : sql_value -> int32
  val sql2int64 : sql_value -> int64
  val sql2float : sql_value -> float
end
