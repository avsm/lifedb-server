(*pp $PP *)

open Utils
open Printf

type json plugin_info = <
   name : string;
   cmd : string;
   mode : string;
   ?period : int option;
   declares: plugin_decl list
>
and plugin_decl = <
   pltype : string;
   description : string;
   implements : string;
   ?icon : string option
>

let plugin_info_file = "LIFEDB_PLUGIN"

let dispatch cgi arg =
    (* initiate a new scan *)
    ()

let scan_plugin_dir db plugin_dir plugin_info_file =
    let info = plugin_info_of_json (Json_io.load_json plugin_info_file) in
    Log.logmod "Plugins" "registering %s (%s)" info#name 
       (String.concat ", " (List.map (fun x -> x#pltype) info#declares));
    List.iter (Sql_mtype_map.update db) info #declares;
    let task = object method name=info#name method cmd=info#cmd method mode=info#mode
        method period=info#period method cwd=Some plugin_dir end in
    with_lock Lifedb_tasks.m (fun () -> Lifedb_tasks.find_or_create_task task)

let do_scan db =
    Log.logmod "Plugins" "Starting scan";
    List.iter (fun dir ->
        let dh = Unix.opendir dir in
        try_final (fun () ->
            repeat_until_eof (fun () ->
                let next_dir = read_next_dir dh in
                let plugin_info = sprintf "%s/%s/%s" dir next_dir plugin_info_file in
                if Sys.file_exists plugin_info then
                    scan_plugin_dir db (Filename.concat dir next_dir) plugin_info
            ) 
        ) (fun () -> Unix.closedir dh);
    ) (Lifedb_config.Dir.plugins ())

let dispatch cgi args = 
    ()
