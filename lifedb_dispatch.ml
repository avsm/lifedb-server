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
open Lifedb_rpc

(* If this is a POST RPC, mark the header as such and retrieve the JSON string *)
let mark_post_rpc (cgi : Netcgi.cgi_activation) =
  let args = cgi#arguments in
  cgi#set_header ~cache:`No_cache ~content_type:"application/json" ();
  if List.length args != 1 then "{}" else (List.hd args)#value

let mark_get_rpc (cgi : Netcgi.cgi_activation) =
  cgi#set_header ~cache:`No_cache ~content_type:"application/json" ()

let mark_delete_rpc (cgi : Netcgi.cgi_activation) =
  cgi#set_header ~cache:`No_cache ()

(* modified version of Netstring.uripath_decode which can handle %2F in URLs *)
let rec uripath_decode s =
  let l = Neturl.split_path s in
  Neturl.join_path (List.map (fun u ->
    let u' = Netencoding.Url.decode ~plus:false u in
    if String.contains u' '/' then
      uripath_decode u'
    else
      u'
  ) l)
 
let dispatch (lifedb : Lifedb_schema.Init.t) (syncdb : Sync_schema.Init.t) env (cgi : Netcgi.cgi_activation) =
  let u = uripath_decode (cgi#url ()) in
  let url = Neturl.parse_url u in
  let url_list = List.filter ((<>) "") (Neturl.url_path url) in
  let url_hd = try List.hd url_list with _ -> "" in
  let url_path = Neturl.join_path (Neturl.url_path url) in
  Log.logmod "URL" "%s %s (%s)" (match cgi#request_method with 
    |`HEAD -> "HEAD" |`GET -> "GET" |`POST -> "POST" |`DELETE -> "DELETE" |_ -> "other")  url_path
    (String.concat " " (List.map (fun a -> sprintf "%s=%s" a#name a#value) cgi#arguments));

  match check_auth cgi with
  (* not authenticated *)
  |false -> begin
    match cgi#request_method, url_hd with
    |_ -> 
      return_need_auth cgi
  end
  (* authenticated *)
  |true -> begin
    match cgi#request_method, url_hd with
    |(`HEAD|`GET), "config" ->
      Lifedb_static.serve_config cgi url_list
    |(`HEAD|`GET), "ping" ->
      cgi#output#output_string "pong";
    |`POST, "mirror" ->
      let arg = mark_post_rpc cgi in
      Sql_mirror.dispatch cgi arg

    |`POST, "intask" ->
      let arg = mark_post_rpc cgi in
      let name = if List.length url_list < 2 then "_unknown" else List.nth url_list 1 in
      Lifedb_tasks.dispatch cgi (`Create (name,arg))
    |`DELETE, "intask" ->
      mark_delete_rpc cgi;
      let name = if List.length url_list < 2 then "_unknown" else List.nth url_list 1 in
      Lifedb_tasks.dispatch cgi (`Destroy name)
    |`GET, "intask" ->
      let tasksel = if List.length url_list < 2 then "_all" else List.nth url_list 1 in
      mark_get_rpc cgi;
      Lifedb_tasks.dispatch cgi (match tasksel with
    |"_all" -> `List
      |name -> `Get name)

    |`GET, "plugin" ->
      let tasksel = if List.length url_list < 2 then "_all" else List.nth url_list 1 in
      mark_get_rpc cgi;
      Lifedb_plugin.dispatch cgi (match tasksel with
        |"_all" -> `List
        |name ->  `Get name)
    |`POST, "plugin" ->
      let tasksel = if List.length url_list < 2 then "_scan" else List.nth url_list 1 in
      ignore(mark_post_rpc cgi);
      if tasksel <> "_scan" then raise (Lifedb_rpc.Resource_not_found "unknown plugin request");
      Lifedb_plugin.dispatch cgi `Scan
    |`POST, "passwd_create" ->
      let arg = mark_post_rpc cgi in
      Lifedb_passwd.dispatch cgi (`Store arg)
    |`POST, "passwd_delete" ->
      let arg = mark_post_rpc cgi in
      Lifedb_passwd.dispatch cgi (`Delete arg)
    |(`GET|`HEAD), "passwd" ->
      mark_get_rpc cgi;
      Lifedb_passwd.dispatch cgi (`Get (List.tl url_list))
    |(`GET|`HEAD), "date" ->
      mark_get_rpc cgi;
      Lifedb_query.dispatch lifedb syncdb env cgi (`Date (List.tl url_list))
    |(`GET|`HEAD), "doc" ->
      mark_get_rpc cgi;
      Lifedb_query.dispatch lifedb syncdb env cgi (`Doc (List.nth url_list 1))
    |(`GET|`HEAD), "pltype" ->
      mark_get_rpc cgi;
      Lifedb_query.dispatch lifedb syncdb env cgi (`Mtype (List.tl url_list))

    |_ -> raise (Invalid_rpc "Unknown request")
  end

let handler lifedb syncdb env cgi =
  let cgi = Netcgi1_compat.Netcgi_types.of_compat_activation cgi in
  cgi#set_header~cache:`No_cache ~content_type:"text/html; charset=\"iso-8859-1\"" ();
  begin try
    Db_thread_access.throttle_request "dispatch" (fun () ->
      dispatch lifedb syncdb env cgi
    )
  with
  |Resource_not_found reason ->
    return_error cgi `Not_found "Resource not found" reason
  |Invalid_rpc reason ->
    return_error cgi `Bad_request "Invalid RPC" reason
  |Resource_conflict reason ->
    return_error cgi `Conflict "Resource conflict" reason
  |error ->
    cgi#output #rollback_work();
    cgi#set_header~status:`Internal_server_error ~cache:`No_cache ~content_type:"text/plain; charset=\"iso-8859-1\"" ();
    cgi#output#output_string "Unexpected software exception:\n";
    cgi#output#output_string (Printexc.to_string error);
    cgi#output#output_string "\n";
  end;
  cgi#output#commit_work()
