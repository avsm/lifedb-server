open Utils
open Printf

let m = Mutex.create ()
let plugin_info_file = "LIFEDB_PLUGIN"
let plugins = Hashtbl.create 1 

let find_plugin name =
    try Some (Hashtbl.find plugins name) with Not_found -> None

let update_mtype db plugin_dir params =
    let name = params#pltype in
    let label = params#description in
    let implements = params#implements in
    let icon = match params#icon with
      |None -> None
      |Some t ->
         let ft = if Filename.is_relative t then
            Filename.concat plugin_dir t
         else t in
         Some ft
    in
    let mtype = match Lifedb_schema.Mtype.get ~name:(Some name) db with
    |[] -> Lifedb_schema.Mtype.t ~name ~label ~implements ~icon db
    |[m] -> m#set_label label; m#set_implements implements; m#set_icon icon; m
    |_ -> assert false in
    ignore(mtype#save)

let scan_plugin_dir db plugin_dir plugin_info_file =
    let info = Lifedb.Rpc.Plugin.t_of_json (Json_io.load_json plugin_info_file) in
    Log.logmod "Plugins" "registering %s (%s)" info#name 
       (String.concat ", " (List.map (fun x -> x#pltype) info#declares));
    List.iter (update_mtype db plugin_dir) info#declares;
    let r = object method name=info#name method cmd=info#cmd method dir=plugin_dir method declares=info#declares  end in
    Hashtbl.add plugins info#name r

let do_scan (db:Lifedb_schema.Init.t) =
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
               cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_tr r))
           |None ->
               Lifedb_rpc.return_error cgi `Not_found "Plugin error" "Plugin not found"
       )
   |`List ->
       with_lock m (fun () ->
           let r = Hashtbl.fold (fun k v a -> v :: a) plugins [] in
           let tr = object method results=List.length r method rows=r end in
           cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_rs tr))
       )
   |`Scan ->
       Db_thread_access.push `Plugins

