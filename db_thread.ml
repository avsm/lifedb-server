(* This thread handles all the database write operations.
   There is a queue which represents "kick" requests from
   other threads. *)

open Utils
open Printf
open Db_thread_access

let db_thread () = 
    let db = new Sql_access.db (Lifedb_config.Dir.lifedb_db()) in
    while true do
        let task = with_lock m (fun () ->
            print_endline "db_thread: waiting";
            dump_q ();
            if Queue.is_empty q then begin
               print_endline "db_thread: empty q, sleeping";
               Condition.wait c m
            end;
            Queue.take q
        ) in
        match task with
        |Lifedb -> Sql_mirror.do_scan db
        |Plugins -> Lifedb_plugin.do_scan db
    done

let start () =
    let _ = Thread.create db_thread () in
    ()