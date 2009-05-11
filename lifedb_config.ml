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

module Dir = struct
    let lifedb_dir = ref "" 
    let cache_dir = ref ""
    let log_dir = ref ""
    let plugins_dir = ref []
    let config_dir = ref ""
    let static_dir = ref ""
    let username_val = ref ""
    let inbox_dir = ref ""

    let lifedb () = !lifedb_dir
    let lifedb_db () = Filename.concat !lifedb_dir "life.db"
    let cache () = !cache_dir
    let uidmap () = Filename.concat !cache_dir "_uidmap"
    let log () = !log_dir
    let plugins () = !plugins_dir
    let config () = !config_dir
    let passwd_db () = Filename.concat !config_dir "passwd.db"
    let sync_db () = Filename.concat !config_dir "sync.db"
    let username () = !username_val
    let static () = !static_dir
    let inbox () = !inbox_dir
end

let port_val = ref 5985
let port () = !port_val

let test_mode_val = ref false
let test_mode () = !test_mode_val

let config_filename_val = ref ""
let config_filename () = !config_filename_val

let root_user () = "root"

let read_config file test_mode =
    let json = Json_io.load_json file in
    let conf = Lifedb.Rpc.Config.t_of_json json in
    let subst = Str.global_substitute (Str.regexp_string "$HOME") (fun _ -> Sys.getenv("HOME")) in
    Dir.lifedb_dir := subst conf#lifedb_directory;
    Dir.plugins_dir := List.map subst conf#plugins_directory;
    Dir.log_dir := subst conf#log_directory;
    Dir.cache_dir := subst conf#cache_directory;
    Dir.config_dir := subst conf#config_directory;
    Dir.static_dir := subst conf#static_directory;
    Dir.inbox_dir := subst conf#inbox_directory;
    Dir.username_val := conf#username;
    port_val := conf#port;
    test_mode_val := test_mode;
    config_filename_val := realpath file

let string_of_config () =
    let json = Lifedb.Rpc.Config.json_of_t (object
       method lifedb_directory = Dir.lifedb ()
       method plugins_directory = Dir.plugins ()
       method log_directory = Dir.log ()
       method cache_directory = Dir.cache ()
       method config_directory = Dir.config ()
       method static_directory = Dir.static ()
       method inbox_directory = Dir.inbox ()
       method username = Dir.username ()
       method port = port ()
    end) in
    Json_io.string_of_json json
