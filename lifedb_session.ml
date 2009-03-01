(*pp $PP *)
(* Authentication and session handling *)

open Printf
open Utils

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

exception Too_many_sessions

let m = Mutex.create ()
let session_table = Hashtbl.create 1

let session_expiry_time = 86400. (* one day *)
let max_sessions = 1000

(* for testing *)
(*
let session_expiry_time = 10. 
let max_sessions = 10
*)

let expire_sessions () =
    let curtime = Unix.gettimeofday () in
    let expired = ref [] in
    Hashtbl.iter (fun session v ->
        if (v.last_accessed +. session_expiry_time) < curtime then begin
            expired := (session,v) :: !expired;
            Hashtbl.remove session_table session;
        end;
    ) session_table;
    !expired

let register_session () =
    let cont = Netplex_cenv.self_cont() in
    (* If we have too many sessions, force some expiry *)
    if Hashtbl.length session_table > max_sessions then begin
       cont#log `Warning "Too many sessions, attempting to expire older ones";
       ignore(expire_sessions ());
       if Hashtbl.length session_table > max_sessions then begin
           cont#log `Warning "Unable to expire sessions, so rejecting login request";
           raise Too_many_sessions
       end;
    end;
    let session_key = Uuidm.to_string ~upper:true (Uuidm.create `V4) in
    let current_time = Unix.gettimeofday () in
    with_lock m (fun () ->
        Hashtbl.add session_table session_key
            { last_accessed = current_time }
    );
    cont#log `Debug (Printf.sprintf "New session: %s" session_key);
    session_key
  
let destroy_session session =
    with_lock m (fun () ->
        Hashtbl.remove session_table session
    )

let dispatch cgi = function
   |`Login p -> begin
       let params = rpc_login_request_of_json (Json_io.json_of_string p) in
        match params#username, params#password with
        |"foo","bar" -> 
            let session = { session = register_session () } in
            cgi#output#output_string (Json_io.string_of_json (json_of_rpc_login_response session))
        |_ ->
            Lifedb_rpc.return_error cgi `Forbidden "Login failed" "Invalid username or password"
   end
   |`Logout session ->
       destroy_session session
       
let check_valid session =
    with_lock m (fun () ->
        try
            let _ = Hashtbl.find session_table session in
            let new_session_info = { last_accessed = Unix.gettimeofday() } in
            Hashtbl.replace session_table session new_session_info;
            true
        with 
        (* Session entry not found *)
        |Not_found -> 
            false
    )

let singleton () =
    let hooks = object
        inherit Netplex_kit.empty_processor_hooks() as super

        val mutable signal_stop = true

        method post_start_hook c =
            super#post_start_hook c;
            ignore(Thread.create (fun () ->
                while signal_stop do
                    c#log `Info (sprintf "Cleaning up session table");
                    let expired = with_lock m expire_sessions in
                    List.iter (fun (sess, v) ->
                        c#log `Debug (sprintf "Expired session: %s (%f)" sess v.last_accessed)
                    ) expired;
                    Thread.delay (session_expiry_time /. 4.);
                done;
                c#log `Debug "Terminating session cleanup thread"
            ) ())
            
        method receive_admin_message c msg args =
            c#log `Info (sprintf "received admin msg %s %s" msg (String.concat "," (Array.to_list args)));
            match msg,args with
            |"session",[|"dump"|] ->
                c#log `Debug "Dumping session table";
                with_lock m (fun () ->
                    Hashtbl.iter (fun k v ->
                        c#log `Debug (sprintf "%s: %f" k v.last_accessed)
                    ) session_table
                )
            |_ -> ()
            
        method shutdown () =
            signal_stop <- false;
            super#shutdown ()
    end in

    object (self)
        method name = "session"
        method create_processor _ _ _ =
            object (self)
            inherit Netplex_kit.processor_base hooks
            method process ~when_done _ _ _ = when_done () (* should not ever be called *)
            method supported_ptypes = [ `Multi_threading ]
        end
    end
