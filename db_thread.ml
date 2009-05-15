(* Copyright (C) 2009 Anil Madhavapeddy <anil@recoil.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

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
    let lifedb' = Lifedb_schema.Init.t (Lifedb_config.Dir.lifedb_db ()) in
    while true do
        let task, copt = with_lock m (fun () ->
            if Queue.is_empty q then begin
               Condition.wait c m;
            end;
            Queue.take q
        ) in
        begin match task with
        |`Lifedb -> Sql_mirror.do_scan lifedb syncdb throttle_check
        |`Plugins -> Lifedb_plugin.do_scan lifedb'
        |`Tasks -> Lifedb_tasks.do_scan ()
        |`Out_tasks -> Lifedb_out_tasks.do_scan ()
        end;
        maybe_signal copt;
    done

let start () =
    ignore(Thread.create db_thread ())
