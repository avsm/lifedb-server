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

open Utils
open Sqlite3
open Printf

module AT=ANSITerminal

let m = Mutex.create ()
let q = Queue.create ()
let c = Condition.create ()

type log_request = [
   |`Module of (string * string)
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
        AT.printf [AT.Foreground AT.Cyan] "[%s] " time;
        print_endline l
    |`Module (m,l) ->
        let time = current_datetime () in
        let col_of_module = function
        |"Tasks" -> AT.Red
        |"Plugins" -> AT.Yellow
        |"Passwd" -> AT.Cyan
        |"RPC" -> AT.Yellow
        |"Sync" -> AT.Blue
        |_ -> AT.Magenta in
        AT.printf [AT.Foreground AT.Cyan] "[%s]" time;
        AT.printf [AT.Foreground (col_of_module m)] "%.10s: " m;
        print_endline l
    |`Plugin (plugin_name, plugin_time, exit_code) ->
        let time = current_datetime () in
        let stmt = db#stmt "inslog" "insert into task_log values(NULL,?,?,?,?)" in
        let exit_code = Int64.of_int exit_code in
        let dbint f = Data.INT (Int64.of_float f) in
        db#transaction (fun () ->
            stmt#bind4 (Data.TEXT plugin_name) (dbint (Unix.gettimeofday())) (dbint plugin_time) (Data.INT exit_code);
            let _ = stmt#step_once in ()
        );
        AT.printf [AT.Foreground AT.Cyan] "[%s]" time;
        AT.printf [AT.Foreground AT.Green] "%.10s: " plugin_name;
        print_endline (sprintf "%f seconds, exit code %Ld" plugin_time exit_code)

let log_thread () =
    let logdbname = Filename.concat (Lifedb_config.Dir.log ()) "log.db" in
    let db = new Sql_access.db logdbname in
    db#exec "create table if not exists
        task_log (id integer primary key autoincrement, name text, time_logged integer, time_taken integer, exit_code integer)";
    while true do
        with_lock m (fun () ->
            if Queue.is_empty q then begin
               Condition.wait c m
            end;
            log_request db (Queue.take q);
        )
    done

let logmod m fmt =
  let xfn f = push (`Module (m, f)) in
  kprintf xfn fmt

let logdbg fmt =
  let xfn f = push (`Debug f) in
  kprintf xfn fmt
    
let init () =
    let _ = Thread.create log_thread () in
    ()
