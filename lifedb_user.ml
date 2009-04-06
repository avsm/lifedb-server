open Utils
open Printf
open Lifedb
module SS=Sync_schema
open Http_client.Convenience

let send_rpc (user:SS.User.t) json =
  let uri = sprintf "http://%s:%Lu/sync" user#ip user#port in
  let string_of_chan cin =
    let buf = Buffer.create 2048 in
    repeat_until_eof (fun () ->
      Buffer.add_string buf (cin#input_line ());
    );
    Buffer.contents buf in
  let process fn =
    try
      let res = fn uri in
      match res#response_status with
      |`Ok -> Some (string_of_chan (res#response_body#open_value_rd ()))
      |_ -> None
    with
      |Http_client.Http_protocol _ -> None
  in
  let post_raw body =
     process (fun uri ->
       http_post_raw_message ~callfn:(fun p ->
          let rh = p#request_header `Base in
          rh#update_field "content-type" "application/json";
          p#set_request_header rh)
        uri body
     ) in
  match post_raw json with
  |Some res -> Log.logmod "User" "Got adduser result: %s" res
  |None -> Log.logmod "User" "FAILED adduser ping"
 
let dispatch db env cgi = function
|`Create arg -> begin
  let u = Rpc.User.t_of_json (Json_io.json_of_string arg) in
  match SS.User.get ~uid:(Some u#uid) db with
  |[] ->
    let user = SS.User.t ~uid:u#uid ~fullname:u#fullname 
      ~ip:u#ip ~port:(Int64.of_int u#port) ~key:u#key db in
    ignore(user#save);
    send_rpc user "hello!";
    ()
  |_ ->
    Lifedb_rpc.return_error cgi `Bad_request "User already exists" "Already registered"
end
|`Delete uid -> begin
  match SS.User.get ~uid:(Some uid) db with
  |[user] -> user#delete
  |_ -> Lifedb_rpc.return_error cgi `Not_found "Unknown user" "User not found"
end

let dispatch_sync lifedb syncdb cgi arg =
  Log.logmod "Sync" "GOT SYNC: %s" arg
