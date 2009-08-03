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
open Lifedb_rpc

module KS = Keychain_schema

(*
 XXX The separate channel here is cludge to avoid some MT issues with the old 
     version of Sqlite in MacOS X base, can be removed with a newer one - avsm *)

let store_passwd db (ctime:float) service username passwd comment =
    Log.logmod "Passwd" "Storing password for service=%s username=%s" service username;
    match Passwords.encrypt_password ctime !Lifedb_rpc.passphrase passwd with
    |Some encpasswd -> begin
        let p = match KS.Passwd.get_by_service_username ~service ~username db with
        |[p] -> Log.logmod "Passwd" "editing"; p#set_ctime ctime; p#set_encpasswd encpasswd; p#set_comment comment; p
        |[] -> Log.logmod "Passwd" "new entry"; KS.Passwd.t ~service ~username ~ctime ~encpasswd ~comment db
        |_ -> assert false in
        ignore(p#save)
    end
    |None -> ()

let get_passwd db service username =
    Log.logmod "Passwd" "Password request for service=%s username=%s" service username;
    match KS.Passwd.get_by_service_username ~service ~username db with
    |[p] -> begin
      match Passwords.decrypt_password p#ctime !Lifedb_rpc.passphrase p#encpasswd with
      |None -> None
      |Some encpasswd -> Some (encpasswd, p)
    end
    |_ -> None

let delete_passwd db service username =
    Log.logmod "Passwd" "Password delete request for service=%s username=%s" service username;
    match KS.Passwd.get_by_service_username ~service ~username db with
    |[p] -> p#delete
    |_ -> ()

let get_all_services db =
    Log.logmod "Passwd" "Password list request";
    List.map (fun p ->
      object
        method service=p#service
        method username=p#username
        method password=""
        method comment=p#comment
      end
    ) (KS.Passwd.get db)

type passwd_req = 
   |Store of float * string * string * Lifedb.Rpc.Plugin.passwd_t
   |Get of string * string
   |List
   |Delete of string * string

let creq = Event.new_channel ()
let cresp = Event.new_channel ()
let crespl = Event.new_channel ()

let lookup_passwd service username =
   Event.sync (Event.send creq (Get (service, username)));
   Event.sync (Event.receive cresp)

let passwd_thread () =
    let db = KS.Init.t (Lifedb_config.Dir.passwd_db ()) in
    while true do
       let e = Event.sync (Event.receive creq) in
       match e with
       |Store (time, service, username, args) -> 
           store_passwd db time service username args#password args#comment
       |Get (service, username) ->
           let r = get_passwd db service username in
           Event.sync (Event.send cresp r)
       |Delete (service, username) ->
           delete_passwd db service username
       |List ->
           Event.sync (Event.send crespl (get_all_services db))
    done

let init () =
    let _ = Thread.create passwd_thread () in ()

let dispatch (cgi:Netcgi.cgi_activation) = function
    |`Create (svc, uname, arg) -> begin
        let ctime = Unix.gettimeofday () in
        let params = Lifedb.Rpc.Plugin.passwd_t_of_json (Json_io.json_of_string arg) in
        Event.sync (Event.send creq (Store (ctime, svc, uname, params)))
    end
    |`Delete (svc, uname) -> begin
        Event.sync (Event.send creq (Delete (svc, uname)))
    end
    |`List ->
        Event.sync (Event.send creq List);
        let rs = Event.sync (Event.receive crespl) in
        cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_passwd_rs (results_of_search rs)));
    |`Get (service, username) -> begin
        match lookup_passwd service username with
        |Some (encpasswd,args) -> 
          let resp = object 
            method service=service 
            method username=username 
            method password=encpasswd 
            method comment=args#comment
          end in
          cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Plugin.json_of_passwd_r resp))
        |None ->
          Lifedb_rpc.return_error cgi `Not_found "Passwd get error" "Service/username not found"
    end
