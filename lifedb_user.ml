open Utils
open Printf
open Lifedb
module LS=Lifedb_schema
module SS=Sync_schema
open Http_client.Convenience

let process fn =
  let string_of_chan cin =
    let buf = Buffer.create 2048 in
    repeat_until_eof (fun () ->
      Buffer.add_string buf (cin#input_line ());
    );
    Buffer.contents buf
  in
  try
    let res = fn () in
    let cin = res#response_body#open_value_rd () in
    Netchannels.with_in_obj_channel cin (fun cin ->
      match res#response_status with
      |`Ok -> `Success (string_of_chan cin)
      |_ -> `Failure (string_of_chan cin)
    )
  with
    |Http_client.Http_protocol _ -> `Failure "unknown"

(* send an RPC to a remote user with the specified json string *) 
let send_rpc (user:SS.User.t) json =
  let uri = sprintf "http://%s:%Lu/sync/%s" user#ip user#port (Lifedb_config.Dir.username ()) in
  let post_raw body =
     process (fun () ->
       http_post_raw_message ~callfn:(fun p ->
          let rh = p#request_header `Base in
          rh#update_field "content-type" "application/json";
          p#set_request_header rh)
        uri body
     ) in
  match post_raw json with
  |`Success res -> Log.logmod "RPC" "-> %s: success (res: %s)" user#uid res
  |`Failure res -> Log.logmod "RPC" "-> %s: epic fail (res: %s)" user#uid res

let succ_sent_guids = Hashtbl.create 1 

(* HTTP PUT some content to a remote user *)
let put_rpc syncdb (p:Http_client.pipeline) (user:SS.User.t) (entry:LS.Entry.t) =
  let uri = sprintf "http://%s:%Lu/sync/%s/_entry/%s" user#ip user#port (Lifedb_config.Dir.username ()) entry#uid in
  let atturi att = sprintf "http://%s:%Lu/sync/%s/_att/%s" user#ip user#port (Lifedb_config.Dir.username ()) (Filename.basename att#file_name) in
  let failure = ref false in
  (* drop the attachments in first *)
  List.iter (fun att ->
    let res = process (fun () ->
      let sz = (Unix.stat att#file_name).Unix.st_size in
      let buf  = Buffer.create sz in
      let fin = open_in att#file_name in
      (try
        Buffer.add_channel buf fin sz;
       with err ->
        close_in fin;
        raise err
      );
      close_in fin;
      let http_call = new Http_client.put (atturi att) (Buffer.contents buf) in
      let hdr = http_call#request_header `Base in
      hdr#update_field "Content-type" att#mime_type;
      hdr#update_field "Content-length" (sprintf "%d" sz);
      http_call#set_request_header hdr;
      p#reset ();
      p#add http_call;
      p#run ();
      http_call
    ) in
    match res with 
    |`Success  res -> Log.logmod "User" "Success uploading attachment %s" att#file_name
    |`Failure res -> Log.logmod "User" "FAILED uploading attachment %s" att#file_name; failure := true
  ) entry#atts;
  if !failure then
    Log.logmod "User" "Had failures uploading attachments, so not doing entry %s" entry#uid
  else begin
    let fin = open_in entry#file_name in
    try_final (fun () ->
      let res = process (fun () ->
        let buf = Buffer.create 2048 in
        repeat_until_eof (fun () -> Buffer.add_string buf (input_line fin));
        let json = Buffer.contents buf in
        let http_call = new Http_client.put uri json in
        let hdr = http_call#request_header `Base in
        hdr#update_field "Content-type" "application/json";
        hdr#update_field "Content-length" (sprintf "%d" (String.length json));
        http_call#set_request_header hdr;
        p#reset ();
        p#add http_call;
        p#run ();
        http_call
      ) in
      match res with
      |`Success res -> Log.logmod "Upload" "Success to %s (%s): %s" user#uid entry#file_name res
      |`Failure res -> Log.logmod "Upload" "Failure to %s (%s): %s" user#uid entry#file_name res)
    (fun () -> close_in fin);
    user#set_sent_guids (add_guids_to_blob user#sent_guids [entry#uid]);
    Hashtbl.replace succ_sent_guids (user#uid, entry#uid) (Unix.gettimeofday ());
    ignore(user#save);
  end

(* Lookup a user UID and apply the function over it *)
let find_user db useruid fn =
  match SS.User.get ~uid:(Some useruid) db with
  |[user] -> fn user
  |_ -> raise (Lifedb_rpc.Resource_not_found "unknown user")

(* Netchannel convenience function to make sure in/out channels are both cleaned up *)
let with_in_and_out_obj_channel cin cout fn =
  Netchannels.with_out_obj_channel cout (fun cout ->
    Netchannels.with_in_obj_channel cin (fun cin ->
      fn cin cout
    )
  )

(* User handling fn, to deal with incoming user create/delete and entry create/delete from
   remote sources *)
let dispatch db env cgi = function
|`Create arg -> begin
  let u = Rpc.User.t_of_json (Json_io.json_of_string arg) in
  match SS.User.get ~uid:(Some u#uid) db with
  |[] ->
    let user = SS.User.t ~uid:u#uid ~ip:u#ip ~port:(Int64.of_int u#port) ~key:u#key ~sent_guids:(blob_of_guids []) ~has_guids:(blob_of_guids []) ~filters:[] ~last_sync:0. db in
    ignore(user#save);
  |_ ->
    Lifedb_rpc.return_error cgi `Bad_request "User already exists" "Already registered"
end
|`Delete uid -> begin
  find_user db uid (fun user -> user#delete)
end
|`Create_filter (useruid,arg) ->
  let f = Rpc.User.filter_of_json (Json_io.json_of_string arg) in
  find_user db useruid (fun user ->
     (* get existing filter list without the currently created one *)
     let f = SS.Filter_rule.t ~name:f#name ~body:f#body ~zorder:(Int64.of_int f#zorder) db in
     ignore(f#save);
     let fs = f :: (List.filter (fun x -> x#name <> f#name) user#filters) in
     user#set_filters fs;
     ignore(user#save);
  )
|`Delete_filter (useruid,name) ->
  find_user db useruid (fun user ->
    let pos,neg = List.partition (fun x -> x#name = name) user#filters in
    match pos with
    |[] -> raise (Lifedb_rpc.Resource_not_found "unknown filter")
    |_ -> user#set_filters neg; ignore(user#save)
  )
|`Entry (arg, useruid, fileuid) -> begin
  find_user db useruid (fun user ->
    let entry_dir = String.concat "/" [Lifedb_config.Dir.inbox (); user#uid; "entries"] in
    let fname = Filename.concat entry_dir (fileuid ^ ".lifeentry") in
    if String.contains fileuid '/' then raise (Lifedb_rpc.Invalid_rpc "bad filename uid");
    if Sys.file_exists fname then raise (Lifedb_rpc.Resource_conflict "attachment already exists");
    make_dirs entry_dir;
    let cout = new Netchannels.output_channel (open_out fname) in
    let cin = arg#open_value_rd  () in
    with_in_and_out_obj_channel cin cout (fun cin cout -> cout#output_channel cin);
    Db_thread_access.throttle_request ();
    Db_thread_access.push `Lifedb;
  )
end
|`Attachment (arg,useruid,fileuid) -> begin
  find_user db useruid (fun user ->
    let att_dir = String.concat "/" [Lifedb_config.Dir.inbox (); user#uid; "_att"] in
    let fname = Filename.concat att_dir fileuid in
    if String.contains fileuid '/' then raise (Lifedb_rpc.Invalid_rpc "bad filename uid");
    make_dirs att_dir;
    let cout = new Netchannels.output_channel (open_out fname) in
    let cin = arg#open_value_rd  () in
    with_in_and_out_obj_channel cin cout (fun cin cout -> cout#output_channel cin)
  )
end

(* upload channel, send it a username/file to upload sequentially *)
let uploadreq = Event.new_channel ()

(* upload contents on the upload queue to remote hosts via HTTP PUT *)
let upload_thread () =
  let lifedb = LS.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = SS.Init.t (Lifedb_config.Dir.sync_db ()) in
  let p = new Http_client.pipeline in
  (*
  let set_verbose_pipeline () =
    let opt = p#get_options in
    p#set_options { opt with 
      Http_client.verbose_status = true;
      verbose_request_header = true;
      verbose_response_header = true;
      verbose_response_contents = true;
      verbose_connection = false
    } in
  set_verbose_pipeline (); *)
  p#set_proxy_from_environment ();
  p#reset ();
  while true do
    let useruid, fileuid = Event.sync (Event.receive uploadreq) in
    Log.logmod "Upload" "Upload request for %s to %s" fileuid useruid;
    match (SS.User.get ~uid:(Some useruid) syncdb), (LS.Entry.get ~uid:(Some fileuid) lifedb)  with
    |[user],[entry] -> begin
       try
         put_rpc syncdb p user entry
       with err ->
         Log.logmod "Sync" "Encountered error syncing %s->%s: %s" user#uid entry#uid (Printexc.to_string err)
    end
    |_ -> Log.logmod "Sync" "WARNING: User %s or entry %s not found" useruid fileuid
  done

(* given a user object, synchronize any entries not present on the remote user host,
   by adding them to the upload thread. *)
let sync_our_entries_to_user lifedb syncdb user =
  Log.logmod "Sync" "Our entries -> %s" user#uid;
  let uids = Lifedb_filter.apply_filters lifedb syncdb user in
  (* filter out recently sent GUIDs from the memory hash *)
  let uids = List.filter (fun e ->
    try
      let tm = Hashtbl.find succ_sent_guids (user#uid,e#uid) in
      Unix.gettimeofday () -. tm > 86400.
    with _ -> true
  ) uids in
  List.iter (fun x -> Log.logmod "Sync" "added upload -> %s: %s" x#uid x#file_name) uids;
  List.iter (fun x -> Event.sync (Event.send uploadreq (user#uid, x#uid))) uids

(* given a user object, send it all the GUIDs we already have to keep it up to date with
   what we might need *)
let sync_our_guids_to_user lifedb syncdb user =
  Log.logmod "Sync" "Our GUIDS -> %s" user#uid;
  let all_guids = LS.Entry.get_uid lifedb in
  let json = Rpc.User.json_of_sync (object method guids=all_guids end) in 
  send_rpc user (Json_io.string_of_json json)

(* thread to regularly iterate over all users and send off our guids *)
let sync_guids_to_remote_users_thread lifedb syncdb =
  let sync_interval = 60. in
  let now = Unix.gettimeofday () in
  List.iter (fun user ->
    if now -. user#last_sync > sync_interval then begin
      sync_our_guids_to_user lifedb syncdb user;
      user#set_last_sync (Unix.gettimeofday());
      ignore(user#save);
    end
  ) (SS.User.get syncdb)

(* thread to listen to received syncs from users and look for entries they
   need to add to the upload thread *)
let sq = Queue.create ()
let sm = Mutex.create ()
let sc = Condition.create ()
let sync_entries_to_remote_users_thread lifedb syncdb =
  with_lock sm (fun () ->
    if Queue.is_empty sq then
      Condition.wait sc sm;
    let useruid = Queue.take sq in
    find_user syncdb useruid (sync_our_entries_to_user lifedb syncdb)
  )

(* received a sync request from another user, so update our has_guids list
 * for that user *)
let dispatch_sync lifedb syncdb cgi uid arg =
  Db_thread_access.throttle_request ();
  match SS.User.get ~uid:(Some uid) syncdb with
  |[] -> Lifedb_rpc.return_error cgi `Forbidden "Unknown user" ""
  |[user] -> 
     let sync = Rpc.User.sync_of_json (Json_io.json_of_string arg) in
     Log.logmod "Sync" "Received GUID update <- %s (%d UIDs)" uid (List.length sync#guids);
     user#set_has_guids (blob_of_guids sync#guids);
     (* XXX reset the sent guids here? what if remote user has deleted and doesnt want them back *)
     user#set_sent_guids (blob_of_guids []); 
     ignore(user#save);
     with_lock sm (fun () ->
       Queue.push user#uid sq;
       Condition.signal sc;
     )
  |_ -> assert false

let thread_with_dbs name fn =
  Thread.delay 5.;
  let lifedb = LS.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = SS.Init.t (Lifedb_config.Dir.sync_db ()) in
  while true do
    (try
      fn lifedb syncdb
    with exn ->
      Log.logmod "Sync" "Got exception in thread '%s': %s" name (Printexc.to_string exn)
    );
    Thread.delay 20.
  done

let init () =
  let _ = Thread.create (thread_with_dbs "remote_guids") sync_guids_to_remote_users_thread in
  let _ = Thread.create (thread_with_dbs "remote_entries") sync_entries_to_remote_users_thread in
  let _ = Thread.create upload_thread () in
  ()

