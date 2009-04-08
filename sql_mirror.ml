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

let summarykey js = 
  let sopt = function
  |None -> None
  |Some x -> Some (sprintf "%d seconds" x) in
  match js#_type with
  |"com.clinklabs.email" -> js#subject
  |"com.apple.iphone.sms" -> js#text
  |"com.apple.iphone.call" -> sopt js#duration
  |"com.skype" -> sopt js#duration
  |"com.twitter" -> js#text
  |"com.adium" -> js#text
  |x -> failwith ("summarykey:" ^ x)

let summaryofmsg js : string =
  match summarykey js with
  |None -> "<none>"
  |Some x -> if String.length x > 50 then String.sub x 0 50 ^ "..." else x

let get_all_mtypes db =
  let h = Hashtbl.create 1 in
  List.iter (fun m -> Hashtbl.add h m#name m) (Mtype.get db);
  h

(* to resolve an attachment, we look for an _att directory one level up, and keep
   looking until we hit the root lifedb directory *)
let resolve_attachments rootdir fname db a =
  (* normalize rootdir *)
  let rootdir = Filename.dirname (Filename.concat rootdir "foo") in
  let rec checkdir bdir =
      let attfname = String.concat "/" [bdir; "_att"; a] in
      let mime_type = Magic_mime.lookup (get_extension attfname) in
      if Sys.file_exists attfname then begin
         let a = match Attachment.get ~file_name:(Some attfname) db with
         |[] -> Attachment.t ~file_name:attfname ~mime_type db 
         |[a] -> a#set_mime_type mime_type; a
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
 
let process_lifeentry db mtypes rootdir fname = 
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
    let e = match Entry.get ~file_name:(Some fname) db with
    |[] ->
      Entry.t ~uid ~file_name:fname ~created:le#_timestamp ~mtype:mtype_info ~from ~recipients ~atts ~tags db
    |[e] ->
      e#set_created le#_timestamp;
      e#set_mtype mtype_info;
      e#set_from from;
      e#set_recipients recipients;
      e#set_atts atts;
      e#set_tags tags;
      e#set_uid uid;
      e
    |_ -> assert false in
    ignore(e#save)

let process_directory db mtypes rootdir dir =
  let dh = opendir dir in
  let counter = ref 0 in
  begin
  try while true do
     incr counter;
     if !counter mod 100 == 0 then (); (* printf ".%!"; *)
     let h = readdir dh in
     if Filename.check_suffix h ".lifeentry" then begin
        let fname = sprintf "%s/%s" dir h in
        try
           process_lifeentry db mtypes rootdir fname
        with e -> 
           Log.logmod "Mirror" "Exception in %s: %s" fname (Printexc.to_string e)
     end
  done with End_of_file -> ()
  end;
  closedir dh
  
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

let check_directory lifedb syncdb mtypes rootdir dir = 
  match dir_needs_update syncdb dir with
  |Some old_mtime  ->
     Log.logmod "Mirror" "Processing %s" dir;
     process_directory lifedb mtypes rootdir dir;
     dir_is_updated syncdb dir old_mtime;
  |None -> ()

let do_scan ?(subdir="") lifedb syncdb =
  Log.logmod "Mirror" "Starting scan";
  let lifedb_path = Filename.concat (Lifedb_config.Dir.lifedb ()) subdir in
  let mtypes = get_all_mtypes lifedb in
  if not (Lifedb_config.test_mode ()) then
      walk_directory_tree lifedb_path (check_directory lifedb syncdb mtypes lifedb_path);
  Log.logmod "Mirror" "Finished scan"

let dispatch cgi args =
  ()
