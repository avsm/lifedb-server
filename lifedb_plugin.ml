(*pp $PP *)

open Utils
open Printf

type json plugin_info = <
   name : string;
   cmd : string;
   declares: plugin_decl list
>
and plugin_decl = <
   pltype : string;
   description : string;
   implements : string;
   ?icon : string option
>
and plugin_rpc = <
   name: string;
   cmd: string;
   declares: plugin_decl list;
   dir: string
>
and plugins = (string, plugin_rpc) Hashtbl.t

type json config_info = <
   name: string;
   plugin: string;
   mode: string;
   silo: string;
   ?period: int option;
   ?secret: config_passwd option;
   ?args: (string , string) Hashtbl.t option
>
and config_passwd = <
   service: string;
   username: string
>

let m = Mutex.create ()
let plugin_info_file = "LIFEDB_PLUGIN"
let plugins = Hashtbl.create 1 


let find_plugin name =
    try Some (Hashtbl.find plugins name) with Not_found -> None

let config_file_extension = ".conf"
exception Plugin_not_found of (string * string)
let scan_config_file db config_file =
    Log.logmod "Plugins" "Scanning config file %s" config_file;
    let task = config_info_of_json (Json_io.load_json config_file) in
    let plugin, plugin_dir = try Hashtbl.find plugins task#plugin
        with Not_found -> raise (Plugin_not_found (config_file, task#name)) in
    let task = object 
        method name=task#name
        method cmd=plugin#cmd 
        method mode=task#mode 
        method period=task#period
        method cwd=Some plugin_dir
        method secret=task#secret
        method args=task#args
        method silo=task#silo
    end in
    with_lock Lifedb_tasks.m (fun () -> Lifedb_tasks.find_or_create_task task)

let scan_plugin_dir db plugin_dir plugin_info_file =
    let info = plugin_info_of_json (Json_io.load_json plugin_info_file) in
    Log.logmod "Plugins" "registering %s (%s)" info#name 
       (String.concat ", " (List.map (fun x -> x#pltype) info#declares));
    List.iter (Sql_mtype_map.update db plugin_dir) info#declares;
    Hashtbl.add plugins info#name (info, plugin_dir)

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
    ) (Lifedb_config.Dir.plugins ());
    let config_dir = Lifedb_config.Dir.config () in
    let dh = Unix.opendir config_dir in
    try_final (fun () ->
        repeat_until_eof (fun () ->
           let next_entry = Unix.readdir dh in
           if Filename.check_suffix next_entry config_file_extension then
              scan_config_file db (Filename.concat config_dir next_entry)
        )
    ) (fun () -> Unix.closedir dh)

let dispatch cgi = function
   |`Get name ->
       with_lock m (fun () ->
           match find_plugin name with
           |Some (pl,_) ->
               cgi#output#output_string (Json_io.string_of_json (json_of_plugin_info pl))
           |None ->
               Lifedb_rpc.return_error cgi `Not_found "Plugin error" "Plugin not found"
       )
   |`List ->
       with_lock m (fun () ->
           let h = Hashtbl.create 1 in
           Hashtbl.iter (fun k (v,dir) -> Hashtbl.add h k (object 
               method name=k method cmd=v#cmd method declares=v#declares method dir=dir
             end)) plugins;
           cgi#output#output_string (Json_io.string_of_json (json_of_plugins h))
       )
   |`Scan ->
       let c = Condition.create () in
       let m' = Mutex.create () in
       Db_thread_access.push (Db_thread_access.Plugins (Some c));
       Condition.wait c m'

