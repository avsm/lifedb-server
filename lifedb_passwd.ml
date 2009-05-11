(*pp $PP *)
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
open Sqlite3

let store_passwd db time service username passwd =
    Log.logmod "Passwd" "Storing password for service=%s username=%s" service username;
    let service' = Data.TEXT service in
    let time' = Data.INT time in
    let username' = Data.TEXT username in
    match Passwords.encrypt_password time !Lifedb_rpc.passphrase passwd with
    |Some encpasswd -> begin
        let encpasswd' = Data.TEXT encpasswd in
        let stmt = db#stmt "checkpass" "select service,username from passwd where service=? and username=?" in
        stmt#bind2 service' username';
        match stmt#step_once with
           |0 ->
               let stmt = db#stmt "inspass" "insert into passwd values(?,?,?,?)" in
               stmt#bind4 service' time' username' encpasswd';
               let _ = stmt#step_once in ()
           |_ ->
               let stmt = db#stmt "uppass" "update passwd set ctime=?,encpasswd=? where service=? and username=?" in
               stmt#bind4 time' encpasswd' service' username'; 
               let _ = stmt#step_once in ()
    end
    |None -> ()

let get_passwd db service username =
    Log.logmod "Passwd" "Password request for service=%s username=%s" service username;
    let service' = Data.TEXT service in
    let username' = Data.TEXT username in
    let stmt = db#stmt "getpass" "select ctime,encpasswd from passwd where service=? and username=?" in 
    stmt#bind2 service' username';
    match stmt#step_once with 
    |0 -> None
    |_ -> 
        let time' = stmt#column 0 in
        let encpasswd' = stmt#column 1 in
        let time = match time' with |Data.INT x -> x |x -> Int64.of_string (Data.to_string x) in
        let encpasswd = Data.to_string encpasswd' in
        Passwords.decrypt_password time !Lifedb_rpc.passphrase encpasswd

let delete_passwd db service username =
    Log.logmod "Passwd" "Password delete request for service=%s username=%s" service username;
    let service' = Data.TEXT service in
    let username' = Data.TEXT username in
    let stmt = db#stmt "delpass" "delete from passwd where service=? and username=?" in
    stmt#bind2 service' username';
    let _ = stmt#step_once in ()

type passwd_req = 
   |Store of int64 * string * string * string
   |Get of string * string
   |Delete of string * string

let creq = Event.new_channel ()
let cresp = Event.new_channel ()

let lookup_passwd service username =
   Event.sync (Event.send creq (Get (service, username)));
   Event.sync (Event.receive cresp)

let passwd_thread () =
    let db = new Sql_access.db (Lifedb_config.Dir.passwd_db ()) in
    db#exec "create table if not exists passwd (service text, ctime integer,
       username text, encpasswd text, primary key (service, username))";
    while true do
       let e = Event.sync (Event.receive creq) in
       match e with
       |Store (time, service, username, password) -> 
           store_passwd db time service username password
       |Get (service, username) ->
           let r = get_passwd db service username in
           Event.sync (Event.send cresp r)
       |Delete (service, username) ->
           delete_passwd db service username
    done

let init () =
    let _ = Thread.create passwd_thread () in ()

type json rpc_passwd_store = <
    service: string;
    username: string;
    password: string
>

type json rpc_passwd_delete = <
    service: string;
    username: string
>

let dispatch (cgi:Netcgi.cgi_activation) = function
    |`Store arg -> begin
        let ctime = Int64.of_float (Unix.gettimeofday ()) in
        let params = rpc_passwd_store_of_json (Json_io.json_of_string arg) in
        Event.sync (Event.send creq (Store (ctime, params#service, params#username, params#password)))
    end
    |`Delete arg -> begin
        let params = rpc_passwd_delete_of_json (Json_io.json_of_string arg) in
        Event.sync (Event.send creq (Delete (params#service, params#username)));
    end
    |`Get url_list -> begin
        match url_list with
        |[service; username] -> begin
            match lookup_passwd service username with
            |Some encpasswd -> 
                 let resp = object method service=service method username=username method password=encpasswd end in
                 cgi#output#output_string (Json_io.string_of_json (json_of_rpc_passwd_store resp))
            |None ->
                 Lifedb_rpc.return_error cgi `Not_found "Passwd get error"
                    "Service/username not found"
        end
        |_ -> Lifedb_rpc.return_error cgi `Bad_request "Passwd get error"
            "Must specify service/username in URL"
    end
