(*pp $PP *)
(* Authentication and session handling *)

type json rpc_login_request = < 
    username : string;
    password : string;
    ?crypto: string option >

type json rpc_login_response = {
    session : string
}

type session_entry = {
    last_accessed: float;
}

let m = Mutex.create ()
let session_table = Hashtbl.create 1

let with_lock fn =
    Mutex.lock m;
    try
        fn ();
        Mutex.unlock m;
    with e ->
        Mutex.unlock m;
        raise e
    
let register_session () =
    let session_key = Uuidm.to_string ~upper:true (Uuidm.create `V4) in
    let current_time = Unix.gettimeofday () in
    with_lock (fun () ->
        Hashtbl.add session_table session_key
            { last_accessed = current_time }
    );
    session_key
    
let dispatch cgi p =
    let params = rpc_login_request_of_json (Json_io.json_of_string p) in
    match params#username, params#password with
    |"foo","bar" -> 
        let session = { session = register_session () } in
        cgi#output#output_string (Json_io.string_of_json (json_of_rpc_login_response session))
    |_ ->
        Lifedb_rpc.return_error cgi `Forbidden "Login failed" "Invalid username or password"
    
let session_expiry_time = 86400. (* one day *)

