open Unix
open Printf
open Utils
open Lifedb
open Lifedb_schema

let walk_directory_tree dir walkfn =
  let rec walk dir =
    walkfn dir;
    let dh = Unix.opendir dir in
    try_final (fun () ->
      repeat_until_eof (fun () ->
         let nextdir = read_next_dir dh in
         if nextdir <> "_att" then
             walk (Filename.concat dir nextdir)
      );
    ) (fun () -> Unix.closedir dh);
  in
  walk dir

type le_type = |Contact |Message
let lifedb_entry_type = function
  |"public.contact" -> Contact
  |"public.message" -> Message
  |x -> failwith (sprintf "unknown implementation type %s" x)

let get_all_mtypes db =
  let h = Hashtbl.create 1 in
  List.iter (fun m -> Hashtbl.add h m#name m) (Mtype.get db);
  h

(* to resolve an attachment, we look for an _att directory one level up, and keep
   looking until we hit the root lifedb directory *)
let resolve_attachments rootdir fname db uid =
  (* normalize rootdir *)
  let rootdir = Filename.dirname (Filename.concat rootdir "foo") in
  let rec checkdir bdir =
      let attfname = String.concat "/" [bdir; "_att"; uid] in
      if Sys.file_exists attfname then begin
         let a = match Attachment.get_by_file_name attfname db with
         |[] ->
            let mime_type = Magic_mime.lookup (get_extension attfname) in
            Attachment.t ~file_name:attfname ~mime_type db 
         |[a] -> a
         |_ -> assert false in
         Some a
      end else begin
          if bdir = rootdir || (String.length bdir < 2) then
              None
          else begin
             checkdir (Filename.dirname bdir)
          end
      end
  in
  checkdir (Filename.dirname fname)
 
let process_lifeentry ~inbox db mtypes rootdir fname = 
  let json = Json_io.load_json ~big_int_mode:true fname in
  let le = Rpc.Entry.t_of_json json in
  let mtype_info = try 
      Hashtbl.find mtypes le#_type
    with Not_found -> begin
       let all_mtypes = String.concat "|" (Hashtbl.fold (fun k v a -> k :: a) mtypes []) in
       failwith (sprintf "unknown mtype %s (we have: %s)" le#_type all_mtypes) 
    end
  in
  let uid = le#_uid in
  match lifedb_entry_type mtype_info#implements with
  |Contact -> begin
    (* Message is a contact *)
    let contact = match Contact.get ~uid:(Some uid) db with
    |[] -> Contact.t ~uid:uid ~first_name:le#first_name ~last_name:le#last_name ~mtime:le#_timestamp ~file_name:fname db
    |[c] -> 
       (* the contact exists, look for the mtime of the existing record *)
       let existing_mtime = (Rpc.Entry.t_of_json (Json_io.load_json ~big_int_mode:true c#file_name))#_timestamp in
       if existing_mtime < le#_timestamp then begin
         Log.logmod "Mirror" "existing mtime is older, so updating record";
         c#set_first_name le#first_name;
         c#set_last_name le#last_name;
         c#set_mtime le#_timestamp;
         c#set_file_name fname;
       end else begin
         Log.logmod "Mirror" "newer contact in db, skipping record";
       end;
       c
    |_ -> assert false in
    let _ = contact#save in
    (* insert the various services in this contact into people fields *)
    let () = match le#_services with
    |None -> ()
    |Some h ->
       Hashtbl.iter (fun service_name service_ids ->
          List.iter (fun service_id ->
            let service = match Service.get ~name:(Some service_name) ~uid:(Some service_id) db with
            |[] -> Service.t ~name:service_name ~uid:service_id ~contact:(Some contact) db
            |[s] -> s#set_contact (Some contact); s
            |_ -> assert false in
            ignore(service#save)
          ) service_ids
        ) h
     in ()
  end
  |Message ->
    (* take an address hash from the JSON and generate, update or retrieve people records and return the people_id *)
    let process_addr addr =
       let service_name = try String.lowercase (List.assoc "type" addr) with Not_found -> failwith "must have type field in addr" in
       let service_id = try String.lowercase (List.assoc "id" addr) with Not_found -> failwith "must have id field in addr" in
       match Service.get ~name:(Some service_name) ~uid:(Some service_id) db with
       |[] -> let s = Service.t ~name:service_name ~uid:service_id ~contact:None db in ignore(s#save); s
       |[s] -> s
       |_ -> assert false in
    let from = match le#_from with |Some addr -> process_addr addr |None -> failwith "no _from in message" in
    let recipients = match le#_to with Some addrs -> List.map process_addr addrs |None -> [] in
    let atts = match le#_att with |None -> []
      |Some a -> List.fold_left (fun acc b -> match resolve_attachments rootdir fname db b with |None -> acc |Some x -> x :: acc) [] a in
    let tags = match le#_tags with |None -> [] |Some tl ->
       List.map (fun name ->
         match Tag.get ~name:(Some name) db with 
         |[tag] -> tag
         |[] -> Tag.t ~name db
         |_ -> assert false) tl in
    (* check if this lifedb entry already exists *)
    let e = match Entry.get_by_file_name fname db with
    |[] ->
      Entry.t ~uid ~inbox ~file_name:fname ~created:le#_timestamp ~mtype:mtype_info ~from ~recipients ~atts ~tags ~delivered:0L db
    |[e] ->
      e#set_created le#_timestamp;
      e#set_mtype mtype_info;
      e#set_from from;
      e#set_recipients recipients;
      e#set_atts atts;
      e#set_tags tags;
      e#set_uid uid;
      (* XXX e#set_delivered? *)
      e
    |_ -> assert false in
    ignore(e#save)

let process_directory ~inbox db throttle_check mtypes rootdir dir =
  let dh = opendir dir in
  try_final (fun () ->
    repeat_until_eof (fun () ->
      throttle_check ();
      let h = readdir dh in
      if Filename.check_suffix h ".lifeentry" then begin
        let fname = sprintf "%s/%s" dir h in
        try
           process_lifeentry ~inbox db mtypes rootdir fname
        with e -> 
           Log.logmod "Mirror" "Exception in %s: %s" fname (Printexc.to_string e)
      end
    )
  ) (fun () -> closedir dh)
  
let dir_needs_update db dir =
  try
    let dir_mtime = (Unix.stat dir).st_mtime in
    match Sync_schema.Dircache.get ~dir:(Some dir) db with
    |[] -> Some dir_mtime
    |[entry] -> if dir_mtime <= entry#mtime then None else Some dir_mtime
    |_ -> assert false
  with Unix.Unix_error _ -> (printf "error!\n"; None)

let dir_is_updated db dir mtime =
  let dir = match Sync_schema.Dircache.get ~dir:(Some dir) db with
  |[] -> Sync_schema.Dircache.t ~dir:dir ~mtime:mtime db
  |[entry] -> entry#set_mtime mtime; entry
  |_ -> assert false in
  ignore(dir#save)

let check_directory ?(inbox=None) lifedb syncdb throttle_check mtypes rootdir dir = 
  match dir_needs_update syncdb dir with
  |Some old_mtime  ->
     Log.logmod "Mirror" "Processing %s" dir;
     process_directory ~inbox lifedb throttle_check mtypes rootdir dir;
     dir_is_updated syncdb dir old_mtime;
  |None -> ()

let do_scan ?(subdir="") lifedb syncdb throttle_check =
  Log.logmod "Mirror" "Starting scan";
  let lifedb_path = Filename.concat (Lifedb_config.Dir.lifedb ()) subdir in
  let inbox_path = Lifedb_config.Dir.inbox () in
  let mtypes = get_all_mtypes lifedb in
  if not (Lifedb_config.test_mode ()) then begin
      walk_directory_tree lifedb_path (check_directory lifedb syncdb throttle_check mtypes lifedb_path);
      if Sys.file_exists inbox_path && (Sys.is_directory inbox_path) then begin
        let dh = opendir inbox_path in
        try_final (fun () ->
          repeat_until_eof (fun () ->
            let folder = read_next_dir dh in
            let inbox_path = Filename.concat inbox_path folder in
            walk_directory_tree inbox_path (check_directory ~inbox:(Some folder) lifedb syncdb throttle_check mtypes inbox_path);
          );
        ) (fun () -> closedir dh)
      end
  end;
  Log.logmod "Mirror" "Finished scan"

let dispatch cgi args =
  ()
