open Printf
open Lifedb_rpc

let dispatch (cgi : Netcgi.cgi_activation) =
    let cont = Netplex_cenv.self_cont() in
    let out = cgi # output # output_string in
    let url = Neturl.parse_url (cgi#url ()) in
    let url_path = Neturl.join_path (Neturl.url_path url) in
    cont#log `Debug (sprintf "Lifedb_rpc: dispatching url=%s" url_path);
    cgi#set_header  ~cache:`No_cache 
        ~content_type:"application/json" ();
    try 
        match url_path with
        |"" -> raise (Invalid_rpc "func not specified")
        |"/login" ->
            if List.length cgi#arguments != 1 then raise (Invalid_rpc "login: POST arguments != 1");
            Lifedb_session.dispatch cgi (List.hd cgi#arguments)#value
        |"/cache" -> begin
           let p = cgi#argument_value "" in
           Lifedb_cache.lockfn (fun () ->
               let v = Lifedb_cache.get () in
               Thread.delay (Random.float 1.);
               let v' = p :: v in
               Lifedb_cache.set v';
               out (String.concat "," v');
           );
           out "\n";
        end 
        |_ -> raise (Invalid_rpc "func unknown")
    with
    |Invalid_rpc reason ->
        return_error cgi `Bad_request "Invalid RPC" reason
