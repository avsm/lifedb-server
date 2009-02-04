(* This is a webserver built from the Netplex and Nethttpd components.
 * It is configured in the netplex.cfg file. 
 * Note: start program with option "-conf netplex.cfg" 
 *)

(**********************************************************************)
(* Dynamic page: The "adder", known from cgi                          *)
(**********************************************************************)

open Netcgi1_compat.Netcgi_types;;
open Printf;;

let generate_page (cgi : Netcgi.cgi_activation) =
  match cgi # request_method with
  |`POST ->
      Lifedb_dispatch.dispatch cgi
  | _ -> 
      cgi # set_header ~status:`Not_implemented
  	  ~cache:`No_cache
  	  ~content_type:"text/plain; charset=\"iso-8859-1\"" ();
  	  cgi # output # output_string "Request method not implemented"


let process2 (cgi : Netcgi.cgi_activation) =
  (* The [try] block catches errors during the page generation. *)
  try
    (* Set the header. The header specifies that the page must not be
     * cached. This is important for dynamic pages called by the GET
     * method, otherwise the browser might display an old version of
     * the page.
     * Furthermore, we set the content type and the character set.
     * Note that the header is not sent immediately to the browser because
     * we have enabled HTML buffering.
     *)
    cgi # set_header 
      ~cache:`No_cache 
      ~content_type:"text/html; charset=\"iso-8859-1\""
      ();

    generate_page cgi;

    (* After the page has been fully generated, we can send it to the
     * browser. 
     *)
    cgi # output # commit_work();
  with
      error ->
	(* An error has happened. Generate now an error page instead of
	 * the current page. By rolling back the output buffer, any 
	 * uncomitted material is deleted.
	 *)
	cgi # output # rollback_work();

	(* We change the header here only to demonstrate that this is
	 * possible.
	 *)
	cgi # set_header 
	  ~status:`Forbidden                  (* Indicate the error *)
	  ~cache:`No_cache 
	  ~content_type:"text/plain; charset=\"iso-8859-1\""
	  ();

	cgi # output # output_string "Software exception:\n";
    cgi # output # output_string (Printexc.to_string error);
    cgi # output # output_string "\n";    
    cgi # output # output_string (Printexc.get_backtrace());
    cgi # output # output_string "\n";
	(* Now commit the error page: *)
	cgi # output # commit_work()
;;


let process1 (cgi : Netcgi1_compat.Netcgi_types.cgi_activation) =
  let cgi' = Netcgi1_compat.Netcgi_types.of_compat_activation cgi in
  process2 cgi'


(**********************************************************************)
(* Create the webserver                                               *)
(**********************************************************************)


let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "Usage: %s [options]" Sys.argv.(0));
  let adder =
    { Nethttpd_services.dyn_handler = (fun _ -> process1);
      dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
      dyn_uri = None;                 (* not needed *)
      dyn_translator = (fun _ -> ""); (* not needed *)
      dyn_accept_all_conditionals = false;
    } in
  let config_cgi = { Netcgi1_compat.Netcgi_env.default_config with
          Netcgi1_compat.Netcgi_env.permitted_input_content_types = [ "application/json" ]
  } in
  
  let nethttpd_factory = 
    Nethttpd_plex.nethttpd_factory
      ~config_cgi: config_cgi
      ~handlers:[ "url_handler", adder ]
      () in
  Random.self_init ();
  Netplex_main.startup
    (Netplex_mt.mt ())
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory ]           (* make this nethttpd available *)
  cmdline_cfg

let _ = 
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    start()
