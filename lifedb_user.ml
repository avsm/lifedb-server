open Utils
open Printf
open Lifedb
module SS=Sync_schema

let dispatch db env cgi = function
|`Create arg -> begin
    let u = Rpc.User.t_of_json (Json_io.json_of_string arg) in
    match SS.User.get ~uid:(Some u#uid) db with
    |[] ->
      let user = SS.User.t ~uid:u#uid ~fullname:u#fullname 
        ~ip:u#ip ~port:(Int64.of_int u#port) ~key:u#key db in
      user#save;
      ()
    |_ -> Lifedb_rpc.return_error cgi `Bad_request "User already exists" "Already registered"
end
