open Printf
open Lifedb_rpc

(* If this is a POST RPC, mark the header as such and retrieve the JSON string *)
let mark_post_rpc (cgi : Netcgi.cgi_activation) =
    let args = cgi#arguments in
    cgi#set_header ~cache:`No_cache ~content_type:"application/json" ();
    if List.length args != 1 then raise (Invalid_rpc "arguments != 1");
    (List.hd args)#value

let mark_get_rpc (cgi : Netcgi.cgi_activation) =
    cgi#set_header ~cache:`No_cache ~content_type:"application/json" ()

let dispatch (cgi : Netcgi.cgi_activation) =
    let u = Nethttp.uripath_decode (cgi#url ()) in
    let url = Neturl.parse_url u in
    let url_list = List.filter ((<>) "") (Neturl.url_path url) in
    let url_hd = try List.hd url_list with _ -> "" in
    let url_path = Neturl.join_path (Neturl.url_path url) in
    Log.logdbg "Lifedb_dispatch: url=%s" url_path;
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
            |`POST, "scan" ->
               let arg = mark_post_rpc cgi in
               Lifedb_plugin.dispatch cgi arg
            |`POST, "mirror" -> begin
               let arg = mark_post_rpc cgi in
               Sql_mirror.dispatch cgi arg
            end
            |`POST, "task_create" -> begin
               let arg = mark_post_rpc cgi in
               Lifedb_tasks.dispatch cgi (`Create arg)
            end
            |`POST, "task_destroy" -> 
               let arg = mark_post_rpc cgi in
               Lifedb_tasks.dispatch cgi (`Destroy arg)
            |`GET, "task" -> begin
               let tasksel = if List.length url_list < 2 then "_all" else List.nth url_list 1 in
               match tasksel with
               |"_all" ->
                   mark_get_rpc cgi; 
                   Lifedb_tasks.dispatch cgi (`List)
               |name ->
                   mark_get_rpc cgi;
                   Lifedb_tasks.dispatch cgi (`Get name)
            end
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
