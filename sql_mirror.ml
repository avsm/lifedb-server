open Unix
open Printf
open Utils
open Lifedb.Rpc

type mtype = {
    m_id: Sqlite3.Data.t;
    m_implements: string;
}

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
  let stmt = db#stmt "all_mtypes" "select id,mtype,implements from mtype_map" in
  stmt#bind0;
  stmt#step_all (fun () ->
      let m = { m_id=stmt#column 0; m_implements=(Sqlite3.Data.to_string (stmt#column 2)) } in
      Hashtbl.add h (Sqlite3.Data.to_string (stmt#column 1)) m
  );
  h

(* to resolve an attachment, we look for an _att directory one level up, and keep
   looking until we hit the root lifedb directory *)
let resolve_attachments rootdir fname lifedbid db a =
  (* normalize rootdir *)
  let rootdir = Filename.dirname (Filename.concat rootdir "foo") in
  let rec checkdir bdir =
      let attfname = String.concat "/" [bdir; "_att"; a] in
      if Sys.file_exists attfname then begin
         (* look for entry in attachments table *)
         let stmt = db#stmt "attsel" "select id from attachments where file_name=?" in
         let fname' = Sqlite3.Data.TEXT attfname in
         stmt#bind1 fname'; 
         (match stmt#step_once with
         |0 -> (* insert *)
             let stmt = db#stmt "attins" "insert into attachments values(NULL,?,?)" in
             stmt#bind2 lifedbid fname';
             let _ = stmt#step_once in ()
         |_ -> (* update *)
             let id = stmt#column 0 in
             let stmt = db#stmt "attup" "update attachments set lifedb_id=?, file_name=? where id=?" in
             stmt#bind3 lifedbid fname' id;
             let _ = stmt#step_once in ()
         );
         Some attfname
      end else begin
          if bdir = rootdir || (String.length bdir < 2) then
              None
          else begin
             checkdir (Filename.dirname bdir)
          end
      end
  in
  ignore(checkdir (Filename.dirname fname))
 
let process_lifeentry db mtypes rootdir fname = 
  let json = Json_io.load_json ~big_int_mode:true fname in
  let le = Entry.t_of_json json in
  let mtype_info = try 
      Hashtbl.find mtypes le#_type
    with Not_found -> begin
       let all_mtypes = String.concat "|" (Hashtbl.fold (fun k v a -> k :: a) mtypes []) in
       failwith (sprintf "unknown mtype %s (we have: %s)" le#_type all_mtypes) 
    end
  in
  match lifedb_entry_type mtype_info.m_implements with
  |Contact -> begin
    (* Message is a contact *)
    let stmt = db#stmt "contactsel" "select id,file_name from contacts where uid=?" in
    let maybe_text = function |Some x -> Sqlite3.Data.TEXT x |None -> Sqlite3.Data.NULL in
    let uid = match le#_uid with 
    |Some x -> Sqlite3.Data.TEXT x |None -> failwith "need _uid field for a contact entry" in
    stmt#bind1 uid;
    let abrecord = maybe_text le#abrecord in
    let firstname = maybe_text le#first_name in
    let lastname = maybe_text le#last_name in
    (* insert or update the contacts field *)
    let contact_id = match stmt#step_once with
    |0 ->
       let insstmt = db#stmt "contactins" "insert into contacts values(NULL,?,?,?,?,?);" in
       insstmt#bind [| (Sqlite3.Data.TEXT fname); uid; abrecord; firstname; lastname |];
       let _ = insstmt#step_once in
       (* return the newly inserted rowid by re-selecting *)
       stmt#bind1 uid;
       let _ = stmt#step_once in
       stmt#column 0
    |_ ->
       (* the contact does indeed exist, look for the mtime of the existing record *)
       let existing_mtime = (Entry.t_of_json (Json_io.load_json ~big_int_mode:true (stmt#str_col 1)))#_timestamp in
       if existing_mtime < le#_timestamp then begin
         Log.logmod "Mirror" "existing mtime is older, so updating record";
         let rowid = stmt#column 0 in
         let stmt = db#stmt "contactup" "update contacts set abrecord=?,first_name=?,last_name=?,file_name=? where id=?" in
         stmt#bind [| abrecord; firstname; lastname; (Sqlite3.Data.TEXT fname); rowid |];
         let _ = stmt#step_once in rowid
       end else begin
         Log.logmod "Mirror" "newer contact in db, skipping record";
         stmt#column 0
       end
    in
    (* insert the various services in this contact into people fields *)
    let () = match le#_services with
    |None -> ()
    |Some h ->
       Hashtbl.iter (fun service_name service_ids ->
          List.iter (fun service_id ->
            let service_name = Sqlite3.Data.TEXT service_name in
            let service_id = Sqlite3.Data.TEXT service_id in
            (* look for this unique (service_name,service_id) tuple in the people table  *)
            let stmt = db#stmt "peoplesel" "select id from people where service_name=? and service_id=?" in
            stmt#bind2 service_name service_id;
            match stmt#step_once with
            |0 ->
              let stmt = db#stmt "peopleins" "insert into people values(NULL,?,?,?)" in
              stmt#bind3 service_name service_id contact_id;
              let _ = stmt#step_once in ()
            |_ ->
              let people_id = stmt#column 0 in
              let stmt = db#stmt "peopleup" "update people set contact_id=? where id=?" in
              stmt#bind2 contact_id people_id;
              let _ = stmt#step_once in ()
          ) service_ids
        ) h
     in ()
  end
  |Message ->
    (* handle message JSON *)
    let mtype = mtype_info.m_id in
    let ctime = Sqlite3.Data.INT (Int64.of_float le#_timestamp) in
    let from_addr = match le#_from with |Some x -> x |None -> failwith "message must have _from" in
    (* take an address hash from the JSON and generate, update or retrieve people records and return the people_id *)
    let process_addr addr =
       let service_name = try Sqlite3.Data.TEXT (String.lowercase (List.assoc "type" addr)) with Not_found -> failwith "must have type field in addr" in
       let service_id = try Sqlite3.Data.TEXT (String.lowercase (List.assoc "id" addr)) with Not_found -> failwith "must have id field in addr" in
       let stmt = db#stmt "peoplesel" "select id from people where service_name=? and service_id=?" in
       stmt#bind2 service_name service_id;
       match stmt#step_once with
       |0 -> begin
          let insstmt = db#stmt "peopleinsnullcid" "insert into people values(NULL,?,?,NULL)" in
          insstmt#bind2 service_name service_id; 
          let _ = insstmt#step_once in
          stmt#bind2 service_name service_id;
          let _ = stmt#step_once in
          stmt#column 0
       end
       |_ -> stmt#column 0
    in
    (* check if this lifedb entry already exists *)
    let stmt = db#stmt "get_lifedb" "select id from lifedb where filename=?" in
    stmt#bind1 (Sqlite3.Data.TEXT fname);
    let people_id = process_addr from_addr in
    let summary = Sqlite3.Data.TEXT (summaryofmsg le) in
    let lifedb_id = match stmt#step_once with
    |0 ->
        (* brand new lifedb record *)
        let insstmt = db#stmt "ins_lifedb" "insert into lifedb values(NULL,?,datetime(?,\"unixepoch\"),?,?,?)" in
        insstmt#bind [| (Sqlite3.Data.TEXT fname); ctime; mtype; people_id; summary |];
        let _ = insstmt#step_once in
        stmt#bind1 (Sqlite3.Data.TEXT fname);
        let _ = stmt#step_once in
        stmt#column 0;
    |_ ->
        let lifedb_id = stmt#column 0 in
        let stmt = db#stmt "up_lifedb" "update lifedb set ctime=?,mtype=?,people_id=?,summary=? where id=?" in
        stmt#bind [|ctime;mtype;people_id;lifedb_id;summary |];
        let _ = stmt#step_once in
        people_id
    in 
    (* resolve the attachments *)
    (match le#_att with |None -> () | Some a -> List.iter (resolve_attachments rootdir fname lifedb_id db) a);
    (* process the to entries *)
    match le#_to with
    |Some addrs ->
      List.iter (fun addr ->
        let people_id = process_addr addr in
        let stmt = db#stmt "lifedb_to_ins" "insert or ignore into lifedb_to values(?,?)" in
        stmt#bind2 lifedb_id people_id;
        let _ = stmt#step_once in ()
      ) addrs 
    |None -> ()

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

let check_directory db syncdb mtypes rootdir dir = 
  match dir_needs_update syncdb dir with
  |Some old_mtime  ->
     Log.logmod "Mirror" "Processing %s" dir;
     db#transaction (fun () ->
         process_directory db mtypes rootdir dir;
         dir_is_updated syncdb dir old_mtime;
     );
  |None -> ()

let init db =
  db#exec "create table if not exists
       dircache (dir text primary key, mtime integer)";
  db#exec "create table if not exists
       lifedb (id integer primary key autoincrement, filename text, ctime integer, mtype integer, people_id integer, summary text)";
  db#exec "create unique index if not exists lifedb_filename on lifedb(filename)";
  db#exec "create table if not exists
       lifedb_to (lifedb_id integer, people_id integer, primary key(lifedb_id, people_id))";
  db#exec "create table if not exists
       contacts (id integer primary key autoincrement, file_name text, uid text, abrecord text, first_name text, last_name text)";
  db#exec "create table if not exists mtype_map (id integer primary key autoincrement, mtype text, 
       label text, icon text, implements text)";
  db#exec "create table if not exists attachments (id integer primary key autoincrement, lifedb_id integer, file_name text)";
  db#exec "create unique index if not exists contacts_uid on contacts(uid)";
  db#exec "create unique index if not exists contacts_filename on contacts(file_name)";
  db#exec "create table if not exists
       people (id integer primary key autoincrement, service_name text, service_id text, contact_id integer)";
  db#exec "create unique index if not exists people_svcid on people(service_name, service_id)"

let do_scan ?(subdir="") (db:Sql_access.db) syncdb =
  Log.logmod "Mirror" "Starting scan";
  let lifedb_path = Filename.concat (Lifedb_config.Dir.lifedb ()) subdir in
  let mtypes = get_all_mtypes db in
  if not (Lifedb_config.test_mode ()) then
      walk_directory_tree lifedb_path (check_directory db syncdb mtypes lifedb_path);
  Log.logmod "Mirror" "Finished scan"

let dispatch cgi args =
  ()
