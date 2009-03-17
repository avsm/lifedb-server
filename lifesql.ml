(* cmd line wrapper to use the SQL mirror from outside daemon *)

let _ =
  Lifedb_config.read_config "config.json";
  Log.init ();
  let db = new Sql_access.db (Lifedb_config.Dir.lifedb_db ()) in
  Sql_mirror.init db;
  Sql_mirror.do_scan db
  

