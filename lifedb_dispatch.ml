open Printf
open Lifedb_rpc

(* If this is an RPC, mark the header as such and retrieve the JSON string *)
let mark_rpc (cgi : Netcgi.cgi_activation) =
    let args = cgi#arguments in
    cgi#set_header ~cache:`No_cache ~content_type:"application/json" ();
    if List.length args != 1 then raise (Invalid_rpc "POST arguments != 1");
    (List.hd args)#value

let dispatch (cgi : Netcgi.cgi_activation) =
    let cont = Netplex_cenv.self_cont() in
    let url = Neturl.parse_url (cgi#url ()) in
    let url_path = Neturl.join_path (Neturl.url_path url) in
    cont#log `Debug (sprintf "Lifedb_rpc: dispatching url=%s" url_path);
    (* check for a valid session *)
    let session = cgi#environment#input_header_field ~default:"" "session" in
    match Lifedb_session.check_valid session with
    (* not authenticated *)
    |false -> begin
        match cgi#request_method, url_path with
        |`POST, "/login" ->
            let arg = mark_rpc cgi in
            Lifedb_session.dispatch cgi (`Login arg)
        |(`HEAD|`GET), "/ping" ->
            return_error cgi `Forbidden "Invalid session" "Login before pinging"
        |_ -> 
            return_error cgi `Forbidden "Invalid session" "No valid session header found"
    end
    (* authenticated *)
    |true -> begin
        try 
            match cgi#request_method, url_path with
            |`POST, "/logout" -> begin
               Lifedb_session.dispatch cgi (`Logout session)
            end
            |(`HEAD|`GET), "/ping" ->
                cgi#output#output_string "pong";
            |`POST, "/mirror" -> begin
               let arg = mark_rpc cgi in
               Sql_mirror.dispatch cgi arg
            end
            |_ -> raise (Invalid_rpc "Unknown request")
        with
        |Invalid_rpc reason ->
            return_error cgi `Bad_request "Invalid RPC" reason
    end
