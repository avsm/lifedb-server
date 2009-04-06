open Utils
open Printf
open Lifedb
module SS=Sync_schema
open Http_client.Convenience

let send_rpc (user:SS.User.t) json =
  let uri = sprintf "http://%s:%Lu/sync/%s" user#ip user#port (Lifedb_config.Dir.username ()) in
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
      |`Ok -> `Success (string_of_chan (res#response_body#open_value_rd ()))
      |_ -> `Failure (string_of_chan (res#response_body#open_value_rd ()))
    with
      |Http_client.Http_protocol _ -> `Failure "unknown"
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
  |`Success res -> Log.logmod "User" "Got adduser result: %s" res
  |`Failure res -> Log.logmod "User" "FAILED adduser ping: %s" res
 
let dispatch db env cgi = function
|`Create arg -> begin
  let u = Rpc.User.t_of_json (Json_io.json_of_string arg) in
  match SS.User.get ~uid:(Some u#uid) db with
  |[] ->
    let user = SS.User.t ~uid:u#uid ~ip:u#ip ~port:(Int64.of_int u#port) ~key:u#key ~sent_guids:[] ~has_guids:[] db in
    ignore(user#save);
    send_rpc user (Json_io.string_of_json (Rpc.User.json_of_sync (object method guids=["foo";"bar";"hello"] end)))
  |_ ->
    Lifedb_rpc.return_error cgi `Bad_request "User already exists" "Already registered"
end
|`Delete uid -> begin
  match SS.User.get ~uid:(Some uid) db with
  |[user] -> user#delete
  |_ -> Lifedb_rpc.return_error cgi `Not_found "Unknown user" "User not found"
end

let dispatch_sync lifedb syncdb cgi uid arg =
  match SS.User.get ~uid:(Some uid) syncdb with
  |[] -> Lifedb_rpc.return_error cgi `Forbidden "Unknown user" ""
  |[user] -> 
     Log.logmod "Sync" "GOT SYNC from %s: %s" uid arg;
     let sync = Rpc.User.sync_of_json (Json_io.json_of_string arg) in
     Log.logmod "Sync" "and some guids: %s" arg;
     user#set_sent_guids (List.map (fun g -> SS.Guid.t ~guid:g syncdb) sync#guids);
     ignore(user#save);
  |_ -> assert false
