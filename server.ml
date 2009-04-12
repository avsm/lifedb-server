open Printf
open Utils
module LD = Lifedb_config.Dir
open Arg

let _ =
  let config_file = ref "config.json" in
  let test_mode = ref false in
  let spec = [
      "-conf", Arg.Set_string config_file, "Name of configuration file to use";
      "-test", Arg.Set test_mode, "Run in test mode";
  ] in
  parse spec (fun _ -> ()) "";

  (* start by reading server configuration *)
  Lifedb_config.read_config !config_file !test_mode;

  (* obtain the master passphrase *)
  let _ = match Platform.get_password (Lifedb_config.root_user ()) with
  |None ->
     prerr_endline (sprintf "Unable to retrieve passphrase for user: %s" (Lifedb_config.root_user ()));
     exit 1;
  |Some p -> 
     Lifedb_rpc.passphrase := p in
  (* start off the stdout logging thread *)
  Log.init ();

  Log.push (`Debug (sprintf "Test mode: %B" !test_mode));

  (* the password handling database thread *)
  Lifedb_passwd.init ();

  (* the task manager thread *)
  Lifedb_tasks.init ();

  (* make and display various directories used by the server *)
  List.iter (fun (a,b) -> 
    Log.push (`Debug (sprintf "%s dir = %s" a b));
    make_dirs b) [ "LifeDB", (LD.lifedb()); "Log", (LD.log()); "Cache", (LD.cache()); "Config", (LD.config()); "Inbox", (LD.inbox()) ];
  Log.push (`Debug (sprintf "Plugin scan dirs = [%s]" (String.concat " | " (LD.plugins()))));

  (* begin the db threads and do a plugin/task scan at start of day *)
  Random.self_init ();
  Db_thread.start ();
  Db_thread_access.push `Plugins;
  Db_thread_access.push `Tasks;
  Db_thread_access.push `Out_tasks;

  (* start the p2p sync thread *)
  Lifedb_user.init ();

  (* start listening to HTTP connections *)
  Http_server.init ()
