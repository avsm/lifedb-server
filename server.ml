open Printf
open Utils
module LD = Lifedb_config.Dir

let _ =
  (* start by reading server configuration *)
  Lifedb_config.read_config "config.json";

  (* obtain the master passphrase *)
  let _ = match Platform.get_password (Lifedb_config.root_user ()) with
  |None ->
     prerr_endline (sprintf "Unable to retrieve passphrase for user: %s" (Lifedb_config.root_user ()));
     exit 1;
  |Some p -> 
     Lifedb_rpc.passphrase := p in
  (* start off the stdout logging thread *)
  Log.init ();

  (* the password handling database thread *)
  Lifedb_passwd.init ();

  (* the task manager thread *)
  Lifedb_tasks.init ();

  (* make and display various directories used by the server *)
  List.iter (fun (a,b) -> 
    Log.push (`Debug (sprintf "%s dir = %s" a b));
    make_dirs b) [ "LifeDB", (LD.lifedb()); "Log", (LD.log()); "Cache", (LD.cache()); "Config", (LD.config()) ];
  Log.push (`Debug (sprintf "Plugin scan dirs = [%s]" (String.concat " | " (LD.plugins()))));

  (* begin the db threads and do a plugin/task scan at start of day *)
  Random.self_init ();
  Db_thread.start ();
  Db_thread_access.push `Plugins;
  Db_thread_access.push `Tasks;

  (* start listening to HTTP connections *)
  Http_server.init ()
