(* cmd line wrapper to use the SQL mirror from outside daemon *)

let _ =
  Lifedb_config.read_config "config.json";
  Log.init ();
  let lifedb = Lifedb_schema.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = Sync_schema.Init.t (Lifedb_config.Dir.sync_db ()) in
  try
    Sql_mirror.do_scan lifedb syncdb;
  with e -> (
    Thread.delay 2.;
    raise e
  )

