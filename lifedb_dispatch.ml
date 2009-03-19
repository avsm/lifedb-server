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

let dispatch (cgi : Netcgi.cgi_activation) =
    let u = Nethttp.uripath_decode (cgi#url ()) in
    let url = Neturl.parse_url u in
    let url_list = List.filter ((<>) "") (Neturl.url_path url) in
    let url_hd = try List.hd url_list with _ -> "" in
    let url_path = Neturl.join_path (Neturl.url_path url) in
    Log.logmod "URL" "%s" url_path;
    match check_auth cgi with
    (* not authenticated *)
    |false -> ()
    |true -> begin
        try 
            match cgi#request_method, url_hd with
            |(`HEAD|`GET), "config" ->
               Lifedb_static.serve_config cgi url_list
            |(`HEAD|`GET), "ping" ->
                cgi#output#output_string "pong";
            |`POST, "mirror" -> begin
               let arg = mark_post_rpc cgi in
               Sql_mirror.dispatch cgi arg
            end
            |`POST, "task" -> begin
               let arg = mark_post_rpc cgi in
               let name = if List.length url_list < 2 then "unknown" else List.nth url_list 1 in
               Lifedb_tasks.dispatch cgi (`Create (name,arg))
            end
            |`DELETE, "task" ->
               mark_delete_rpc cgi;
               let name = if List.length url_list < 2 then "unknown" else List.nth url_list 1 in
               Lifedb_tasks.dispatch cgi (`Destroy name)
            |`GET, "task" ->
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
               (match tasksel with
               |"_scan" -> Lifedb_plugin.dispatch cgi `Scan
               |_ -> return_error cgi `Not_found "Unknown plugin request" "Only _scan is valid")
            |`POST, "passwd_create" -> begin
               let arg = mark_post_rpc cgi in
               Lifedb_passwd.dispatch cgi (`Store arg)
            end
            |`POST, "passwd_delete" -> begin
               let arg = mark_post_rpc cgi in
               Lifedb_passwd.dispatch cgi (`Delete arg)
            end
            |(`GET|`HEAD), "passwd" -> begin
               mark_get_rpc cgi;
               Lifedb_passwd.dispatch cgi (`Get (List.tl url_list))
            end
            |_ -> raise (Invalid_rpc "Unknown request")
        with
        |Invalid_rpc reason ->
            return_error cgi `Bad_request "Invalid RPC" reason
    end
