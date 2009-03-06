open Netcgi1_compat.Netcgi_types
open Printf
open Utils

module LD = Lifedb_config.Dir

let process2 (cgi : Netcgi.cgi_activation) =
  try
    cgi#set_header~cache:`No_cache 
      ~content_type:"text/html; charset=\"iso-8859-1\""
      ();

    Lifedb_dispatch.dispatch cgi;

    cgi#output#commit_work();
  with
  |error ->
	cgi#output #rollback_work();
	cgi#set_header~status:`Internal_server_error
	  ~cache:`No_cache ~content_type:"text/plain; charset=\"iso-8859-1\""
	  ();

	cgi # output # output_string "Unexpected software exception:\n";
    cgi # output # output_string (Printexc.to_string error);
    cgi # output # output_string "\n";    
    cgi # output # output_string (Printexc.get_backtrace());
    cgi # output # output_string "\n";
	cgi # output # commit_work()


let process1 (cgi : Netcgi1_compat.Netcgi_types.cgi_activation) =
  let cgi' = Netcgi1_compat.Netcgi_types.of_compat_activation cgi in
  process2 cgi'

let start() =
  Lifedb_config.read_config "config.json";
  Log.init ();
  printf "LifeDB dir: %s\n Plugins dir:%s\n Log dir:%s\nCache dir:%s\n%!" 
    (LD.lifedb()) (String.concat "|" (LD.plugins())) (LD.log()) (LD.cache());
  List.iter make_dirs ((LD.lifedb()) :: (LD.log()) :: (LD.cache()) :: (LD.plugins()) );

  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "Usage: %s [options]" Sys.argv.(0));
  let http_handler =
    { Nethttpd_services.dyn_handler = (fun _ -> process1);
      dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
      dyn_uri = None;                 (* not needed *)
      dyn_translator = (fun _ -> ""); (* not needed *)
      dyn_accept_all_conditionals = false;
    } in
  let config_cgi = { Netcgi1_compat.Netcgi_env.default_config with
          Netcgi1_compat.Netcgi_env.permitted_input_content_types = 
            [ "application/json"; "application/x-www-form-urlencoded" ]
  } in
  
  let nethttpd_factory = 
    Nethttpd_plex.nethttpd_factory
      ~config_cgi: config_cgi
      ~handlers:[ "url_handler", http_handler ]
      () in
  Random.self_init ();
  
  let session_factory = Lifedb_session.singleton () in
  let task_factory = Lifedb_tasks.singleton () in
  Db_thread.start ();
  Db_thread_access.push Db_thread_access.Plugins;
  Netplex_main.startup
    (Netplex_mt.mt ())
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory; session_factory; task_factory ] 
  cmdline_cfg

let _ = 
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    start()
