(* $Id: dbf_mysql.mli 149 2006-01-13 12:06:44Z zoggy $ *)

val debug_printer : (string -> unit) ref

module MysqlDriver : Dbf_sql_driver.SqlDriver
