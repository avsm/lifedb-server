(* This thread handles all the database write operations.
   There is a queue which represents "kick" requests from
   other threads. *)

open Utils
open Printf
open Db_thread_access

let maybe_signal = function
    |None -> ()
    |Some c -> Condition.signal c

let db_thread () = 
    let db = new Sql_access.db (Lifedb_config.Dir.lifedb_db()) in
    Sql_mirror.init db;
    while true do
        let task, copt = with_lock m (fun () ->
            if Queue.is_empty q then begin
               Condition.wait c m;
            end;
            Queue.take q
        ) in
        begin match task with
        |`Lifedb -> Sql_mirror.do_scan db
        |`Plugins -> Lifedb_plugin.do_scan db
        |`Tasks -> Lifedb_tasks.do_scan ()
        end;
        maybe_signal copt;
    done

let start () =
    ignore(Thread.create db_thread ())
