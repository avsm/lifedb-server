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
    let lifedb = Lifedb_schema.Init.t (Lifedb_config.Dir.lifedb_db ()) in
    let syncdb = Sync_schema.Init.t (Lifedb_config.Dir.sync_db ()) in
    while true do
        let task, copt = with_lock m (fun () ->
            if Queue.is_empty q then begin
               Condition.wait c m;
            end;
            Queue.take q
        ) in
        begin match task with
        |`Lifedb -> Sql_mirror.do_scan lifedb syncdb
        |`Plugins -> Lifedb_plugin.do_scan lifedb
        |`Tasks -> Lifedb_tasks.do_scan ()
        |`Out_tasks -> Lifedb_out_tasks.do_scan ()
        end;
        maybe_signal copt;
    done

let start () =
    ignore(Thread.create db_thread ())
