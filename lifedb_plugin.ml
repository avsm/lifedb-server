(*pp $PP *)

open Utils
open Printf

let m = Mutex.create ()
let plugin_info_file = "LIFEDB_PLUGIN"
let plugins = Hashtbl.create 1 

let find_plugin name =
    try Some (Hashtbl.find plugins name) with Not_found -> None

let scan_plugin_dir db plugin_dir plugin_info_file =
    let info = Lifedb.Rpc.Plugin.t_of_json (Json_io.load_json plugin_info_file) in
    Log.logmod "Plugins" "registering %s (%s)" info#name 
       (String.concat ", " (List.map (fun x -> x#pltype) info#declares));
    List.iter (Sql_mtype_map.update db plugin_dir) info#declares;
    let r = object method info=info method dir=plugin_dir end in
    Hashtbl.add plugins info#name r

let do_scan db =
    Hashtbl.clear plugins;
    Log.logmod "Plugins" "Starting scan";
    List.iter (fun dir ->
        try
            let dh = Unix.opendir dir in
            try_final (fun () ->
                repeat_until_eof (fun () ->
                    let next_dir = read_next_dir dh in
                    let plugin_info = sprintf "%s/%s/%s" dir next_dir plugin_info_file in
                    if Sys.file_exists plugin_info then
                        scan_plugin_dir db (Filename.concat dir next_dir) plugin_info
                ) 
            ) (fun () -> Unix.closedir dh);
        with Unix.Unix_error _ -> Log.logmod "Plugins" "Skipping directory: %s" dir
    ) (Lifedb_config.Dir.plugins ())

let dispatch cgi = function
   |`Get name ->
       with_lock m (fun () ->
           match find_plugin name with
           |Some r ->
               cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_r r))
           |None ->
               Lifedb_rpc.return_error cgi `Not_found "Plugin error" "Plugin not found"
       )
   |`List ->
       with_lock m (fun () ->
           cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_ts plugins))
       )
   |`Scan ->
       Db_thread_access.push_sync `Plugins

