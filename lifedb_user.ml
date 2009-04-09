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
  |`Success res -> Log.logmod "User" "Got adduser result: %s" res
  |`Failure res -> Log.logmod "User" "FAILED adduser ping: %s" res

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
    let find_guid = match SS.Guid.get ~guid:(Some entry#uid) syncdb with
    |[] -> SS.Guid.t ~guid:entry#uid syncdb
    |[guid] -> guid
    |_ -> assert false in
    user#set_sent_guids (find_guid :: user#sent_guids);
    ignore(user#save);
  end

let find_user db useruid fn =
  match SS.User.get ~uid:(Some useruid) db with
  |[user] -> fn user
  |_ -> raise (Lifedb_rpc.Resource_not_found "unknown user")

let with_in_and_out_obj_channel cin cout fn =
  Netchannels.with_out_obj_channel cout (fun cout ->
    Netchannels.with_in_obj_channel cin (fun cin ->
      fn cin cout
    )
  )

let dispatch db env cgi = function
|`Create arg -> begin
  let u = Rpc.User.t_of_json (Json_io.json_of_string arg) in
  match SS.User.get ~uid:(Some u#uid) db with
  |[] ->
    let user = SS.User.t ~uid:u#uid ~ip:u#ip ~port:(Int64.of_int u#port) ~key:u#key ~sent_guids:[] ~has_guids:[] ~last_sync:0. db in
    ignore(user#save);
  |_ ->
    Lifedb_rpc.return_error cgi `Bad_request "User already exists" "Already registered"
end
|`Delete uid -> begin
  find_user db uid (fun user -> user#delete)
end
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

let upload_thread () =
  let lifedb = LS.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = SS.Init.t (Lifedb_config.Dir.sync_db ()) in
  let p = new Http_client.pipeline in
  let set_verbose_pipeline () =
    let opt = p#get_options in
    p#set_options { opt with 
      Http_client.verbose_status = true;
      verbose_request_header = true;
      verbose_response_header = true;
      verbose_response_contents = true;
      verbose_connection = false
    } in
  set_verbose_pipeline ();
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

let sync_user lifedb syncdb user =
  Log.logmod "Sync" "sync_user: %s (has=%s)" user#uid (String.concat "," (List.map (fun e -> e#guid) user#has_guids));
  (* filter criteria hardcoded to all things of mtype com.apple.iphoto with a _to of the user *)
  let all_guids = LS.Entry.get lifedb in
  let photo_guids = List.filter (fun e -> e#mtype#name = "com.apple.iphoto") all_guids in
  let guids_for_user = List.filter (fun e ->
    List.length (
      List.find_all (fun s ->
        s#name = "email" && s#uid = user#uid
      ) e#recipients
    ) > 0) photo_guids in
  (* if we've already sent them or user has them already, then filter them out *)
  let has_uids = List.map (fun g -> g#guid) (user#has_guids @ user#sent_guids) in
  Log.logmod "User" "user has_uids=%s  for_user=(%s)" (String.concat ", " has_uids) (String.concat ", " (List.map (fun g -> g#uid) guids_for_user));
  let filtered_guids_for_user = List.filter (fun e ->
    not (List.mem e#uid has_uids)
  ) guids_for_user in
  List.iter (fun x -> Log.logmod "Sync" "%s %s" x#uid x#file_name) filtered_guids_for_user;
  List.iter (fun x -> Event.sync (Event.send uploadreq (user#uid, x#uid))) filtered_guids_for_user
  
let sync_thread () =
  Thread.delay 5.;
  let sync_interval = 60. in
  let lifedb = LS.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = SS.Init.t (Lifedb_config.Dir.sync_db ()) in
  while true do
     let now = Unix.gettimeofday () in
     List.iter (fun user ->
       Log.logmod "Sync" "sync: %s   %.2f - %.2f" user#uid now user#last_sync;
       if now -. user#last_sync > sync_interval then begin
         sync_user lifedb syncdb user;
         user#set_last_sync (Unix.gettimeofday());
         ignore(user#save);
       end
     ) (SS.User.get syncdb);
     Thread.delay 20.;
  done

let init () =
  let _ = Thread.create sync_thread () in
  let _ = Thread.create upload_thread () in
  ()

let dispatch_sync lifedb syncdb cgi uid arg =
  match SS.User.get ~uid:(Some uid) syncdb with
  |[] -> Lifedb_rpc.return_error cgi `Forbidden "Unknown user" ""
  |[user] -> 
     Log.logmod "Sync" "GOT SYNC from %s: %s" uid arg;
     let sync = Rpc.User.sync_of_json (Json_io.json_of_string arg) in
     Log.logmod "Sync" "and some guids: %s" arg;
     user#set_has_guids (List.map (fun g -> SS.Guid.t ~guid:g syncdb) sync#guids);
     ignore(user#save);
   (*  request_sync uid; *)
  |_ -> assert false



