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

open Printf
open Utils
module LD = Lifedb_config.Dir
open Arg

let _ =
  let config_file = ref "config.json" in
  let test_mode = ref false in
  let spec = [
      "-conf", Arg.Set_string config_file, "Name of configuration file to use";
      "-test", Arg.Set test_mode, "Run in test mode";
  ] in
  parse spec (fun _ -> ()) "";

  (* start by reading server configuration *)
  Lifedb_config.read_config !config_file !test_mode;

  (* obtain the master passphrase *)
  let _ = match Platform.get_password (Lifedb_config.root_user ()) with
  |None ->
     prerr_endline (sprintf "Unable to retrieve passphrase for user: %s" (Lifedb_config.root_user ()));
     exit 1;
  |Some p -> 
     Lifedb_rpc.passphrase := p in
  (* start off the stdout logging thread *)
  Log.init ();

  Log.push (`Debug (sprintf "Test mode: %B" !test_mode));

  (* the password handling database thread *)
  Lifedb_passwd.init ();

  (* the task manager thread *)
  Lifedb_tasks.init ();

  (* make and display various directories used by the server *)
  List.iter (fun (a,b) -> 
    Log.push (`Debug (sprintf "%s dir = %s" a b));
    make_dirs b) [ "LifeDB", (LD.lifedb()); "Log", (LD.log()); "Cache", (LD.cache()); "Config", (LD.config()) ];
  Log.push (`Debug (sprintf "Plugin scan dirs = [%s]" (String.concat " | " (LD.plugins()))));

  (* begin the db threads and do a plugin/task scan at start of day *)
  Random.self_init ();
  Db_thread.start ();
  Db_thread_access.push `Plugins;
  Db_thread_access.push `Tasks;

  (* start listening to HTTP connections *)
  Http_server.init ()
