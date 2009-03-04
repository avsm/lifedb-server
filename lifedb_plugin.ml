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
   implements : string list;
   ?icon : string option
>

let plugin_info_file = "LIFEDB_PLUGIN"

let m = Mutex.create ()
let c = Condition.create ()

let dispatch cgi arg =
    (* initiate a new scan *)
    ()

let kick_scan_thread () =
    with_lock m (fun () -> 
        print_endline "kick_scan_thread";
        Condition.signal c;
    )

let scan_plugin_dir plugin_dir plugin_info_file =
    let info = plugin_info_of_json (Json_io.load_json plugin_info_file) in
    printf "pl=%s mode=%s cmd=%s\n%!" plugin_dir info#mode info#cmd;
    List.iter (fun decl ->
        printf "pltype: %s   descr: %s\nimplements: %s\n%!" decl#pltype decl#description 
          (String.concat ", " decl#implements);
    ) info#declares;
    let task = object method name=info#name method cmd=info#cmd method mode=info#mode
        method period=info#period method cwd=Some plugin_dir end in
    with_lock Lifedb_tasks.m (fun () -> Lifedb_tasks.find_or_create_task task)

let scan_thread plugins_dirs =
    print_endline "scan_thread: start";
    while true do
        with_lock m (fun () ->
            Condition.wait c m;
            print_endline "scan_thread: woke up";
            List.iter (fun dir ->
              let dh = Unix.opendir dir in
              try_final (fun () ->
                 repeat_until_eof (fun () ->
                   let next_dir = read_next_dir dh in
                   print_endline (sprintf "scanning: %s" next_dir);
                   let plugin_info = sprintf "%s/%s/%s" dir next_dir plugin_info_file in
                   if Sys.file_exists plugin_info then
                       scan_plugin_dir (Filename.concat dir next_dir) plugin_info
                 ) 
              ) (fun () -> Unix.closedir dh);
              print_endline "scan_thread: done";
           ) plugins_dirs
        )
    done

let start () = 
    let _ = Thread.create scan_thread (Lifedb_config.Dir.plugins()) in
    Thread.delay 3.;
    kick_scan_thread ()

let dispatch cgi args = 
    ()
