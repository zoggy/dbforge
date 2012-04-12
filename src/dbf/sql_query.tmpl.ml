<# [
open Dbf_sql.SQL_db
open Dbf_sql
] #>

<?block [
let query   = tmpl__env in
let state = query_state query in

let (return_types, params, qtree) =
  match state with
    Query_ok (t,l) ->
      (t,l,Sqml.query_of_string query.qry_query)
  | e ->
      failwith (Printf.sprintf "query %s: %s"
		  query.qry_name
		  (string_of_query_state state)
	       )
in
let return_types =
  let a = Array.mapi (fun i col -> (Printf.sprintf "ret%d" i, col))
      (Array.of_list return_types)
  in
  Array.to_list a
in
let concr = Sqml_pp.make_pp_query qtree in
let pp_env = Hashtbl.create 13 in
let _ =
  List.iter
    (fun (name,colopt) ->
      let s =
	match colopt with
	  None -> name
	| Some col ->
	    Printf.sprintf "(%s %s)"
	      col.col_ml2sql name
      in
      Hashtbl.add pp_env name
	(Printf.sprintf "\"^(Sql.escape_value %s)^\"" s)
    )
    params
]?>

let <![query.qry_name]!> = fun db
    <?iter:name=p [params]?>~<![fst p]!> <?/iter?> ->
  let query =
    "<![Dbf_misc.strip_string
      (Sqml_pp.pp_concr ~escape_dblquotes: true 2 (Some pp_env) concr)]!>" in
  match Sql.exec db query with
    | Sql.R_Ok
    | Sql.R_Empty -> []
    | Sql.R_Fetch cursor ->
	let f = function
	  | [|<![String.concat ";" (List.map fst return_types)]!>|] ->
	    <?iter:name=t [return_types]?>
	      <![let (name,colopt) = t in
	        match colopt with
		  None -> ""
		| Some col ->
		    Printf.sprintf "let %s = %s in\n" name
		      (
		       if col.col_nullable then
			 Printf.sprintf "Dbf_sql_misc.apply_opt %s %s"
			   col.col_sql2ml
			   name
		       else
			 Printf.sprintf "%s (Dbf_sql_misc.unopt %s)"
			   col.col_sql2ml
			   name
		      )
	     ]!>
	    <?/iter?>
	    (<![String.concat "," (List.map fst return_types)]!>)
	  | _ -> assert false
	in
	let to_array = function
          | Sql.FR_Array a -> a
          | _ -> Dbf_sql_misc.ie ()
	in
	Sql.map cursor ~f: (fun r -> f (to_array r))
<?/block?>
