open Utils
open Sqlite3
open Printf

let m = Mutex.create ()
let q = Queue.create ()
let c = Condition.create ()

type log_request = [
   |`Debug of string
   |`Plugin of (string * float * int)
]

let push (l:log_request) =
    with_lock m (fun () ->
        Queue.push l q;
        Condition.signal c;
    )

let log_request db = function
    |`Debug l ->
        let time = current_datetime () in
        print_endline (sprintf "[%s] %s" time l);
    |`Plugin (plugin_name, plugin_time, exit_code) ->
        let time = current_datetime () in
        let stmt = db#stmt "inslog" "insert into task_log values(NULL,?,?,?,?)" in
        let exit_code = Int64.of_int exit_code in
        let dbint f = Data.INT (Int64.of_float f) in
        db#transaction (fun () ->
            stmt#bind4 (Data.TEXT plugin_name) (dbint (Unix.gettimeofday())) (dbint plugin_time) (Data.INT exit_code);
            let _ = stmt#step_once in ()
        );
        print_endline (sprintf "PLUGIN %s: [%s] %f seconds, exit code %Ld" plugin_name time plugin_time exit_code)

let log_thread () =
    let logdbname = Filename.concat (Lifedb_config.Dir.log ()) "log.db" in
    let db = new Sql_access.db logdbname in
    db#exec "create table if not exists
        task_log (id integer primary key autoincrement, name text, time_logged integer, time_taken integer, exit_code integer)";
    while true do
        let l = with_lock m (fun () ->
            if Queue.is_empty q then begin
               Condition.wait c m
            end;
            Queue.take q
        ) in
        log_request db l;
    done
    
let init () =
    let _ = Thread.create log_thread () in
    ()