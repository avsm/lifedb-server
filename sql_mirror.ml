(*pp $PP *)

type json lifeentry = {
    _type: string;
    _timestamp: float;
    ?_uid : string option;
    ?abrecord: string option;
    ?_from: addr option;
    ?_to: addr list option;
    ?first_name: string option;
    ?last_name: string option;
    ?_services: (string, string list) Hashtbl.t option;
    ?subject: string option;
    ?duration: int option;
    ?text: string option
} and addr = (string * string) assoc

(* read next directory in an open dir_handle *)
external read_next_dir : Unix.dir_handle -> string = "unix_read_next_dir"

open Unix
open Printf

let try_final fn finalfn =
  begin
  try fn ()
  with e -> (finalfn(); raise e) end;
  finalfn ()

let repeat_until_eof fn =
   try while true do
      fn ()
   done 
   with End_of_file -> ()

let walk_directory_tree dir walkfn =
  let rec walk dir =
    walkfn dir;
    let dh = Unix.opendir dir in
    try_final (fun () ->
      repeat_until_eof (fun () ->
         let nextdir = read_next_dir dh in
         walk (Filename.concat dir nextdir)
      );
    ) (fun () -> Unix.closedir dh);
  in
  walk dir

type le_type = |Contact |Message
let lifedb_entry_type = function
  |"com.clinklabs.contact" -> Contact
  |_ -> Message

let maplabelfn = function
  |"com.clinklabs.email" -> "Email"
  |"com.apple.iphone.sms" -> "SMS"
  |"com.apple.iphone.call" -> "Phone Call"
  |"com.skype" -> "Skype"
  |"com.twitter" -> "Twitter"
  |"com.adium" -> "Instant Messaging"
  |x -> x

let summarykey js = 
  let sopt = function
  |None -> None
  |Some x -> Some (sprintf "%d seconds" x) in
  match js._type with
  |"com.clinklabs.email" -> js.subject
  |"com.apple.iphone.sms" -> js.text
  |"com.apple.iphone.call" -> sopt js.duration
  |"com.skype" -> sopt js.duration
  |"com.twitter" -> js.text
  |"com.adium" -> js.text
  |x -> failwith ("summarykey:" ^ x)

let summaryofmsg js : string =
  match summarykey js with
  |None -> "<none>"
  |Some x -> if String.length x > 50 then String.sub x 0 50 ^ "..." else x

let process_lifeentry db rootdir fname = 
  let json = Json_io.load_json ~big_int_mode:true fname in
  let le = lifeentry_of_json json in
  match lifedb_entry_type le._type with
  |Contact -> begin
    (* Message is a contact *)
    let stmt = db#stmt "contactsel" "select id from contacts where uid=?" in
    (* XXX check mtime below and ignore if older than current *)
    let maybe_text = function |Some x -> Sqlite3.Data.TEXT x |None -> Sqlite3.Data.NULL in
    let uid = match le._uid with 
    |Some x -> Sqlite3.Data.TEXT x |None -> failwith "need _uid field for a contact entry" in
    stmt#bind1 uid;
    let abrecord = maybe_text le.abrecord in
    let firstname = maybe_text le.first_name in
    let lastname = maybe_text le.last_name in
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
       let rowid = stmt#column 0 in
       let stmt = db#stmt "contactup" "update contacts set abrecord=?,first_name=?,last_name=? where id=?" in
       stmt#bind [| abrecord; firstname; lastname; rowid |];
       let _ = stmt#step_once in rowid
    in
    (* insert the various services in this contact into people fields *)
    begin
    match le._services with
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
    end
  end
  |Message ->
    (* handle message JSON *)
    let mtype = db#lookup_mapping le._type maplabelfn in
    let ctime = Sqlite3.Data.INT (Int64.of_float le._timestamp) in
    let from_addr = match le._from with |Some x -> x |None -> failwith "message must have _from" in
    (* take an address hash from the JSON and generate, update or retrieve people records and return the people_id *)
    (* cases for the addr handling:
       - brand new person record for (svcname,svcid)
       -- no contact_id from the db in this case, but could use the one in the file (A)
       -- no contact_id in the file, so leave it NULL (B)
       - existing person record for (svcname,svcid) (C)
       -- if existing contact_id is NULL, set it to the one in the file (D)
       -- if existing contact_id is not null and file is null, change the file (E)
       -- if existing contact_id is not null, and file is not null: (F)
       --- if they are different, signal a conflict (G)
       --- if they are the same, just continue  (H)
    *)
    let process_addr addr =
       (* does this contact "uid" field exist in the database? resolve into a contact_id integer or NULL *)
       let contact_id_in_file = try
           let uid = Sqlite3.Data.TEXT (List.assoc "uid" addr) in
           let contstmt = db#stmt "contidsel" "select id from contacts where uid=?" in
           contstmt#bind1 uid;
           match contstmt#step_once with
           |0 -> Sqlite3.Data.NULL
           |_ -> contstmt#column 0
         with Not_found -> Sqlite3.Data.NULL in
       let service_name = try Sqlite3.Data.TEXT (String.lowercase (List.assoc "type" addr)) with Not_found -> failwith "must have type field in addr" in
       let service_id = try Sqlite3.Data.TEXT (String.lowercase (List.assoc "id" addr)) with Not_found -> failwith "must have id field in addr" in
       let stmt = db#stmt "peoplesel" "select id,contact_id from people where service_name=? and service_id=?" in
       stmt#bind2 service_name service_id;
       match stmt#step_once with
       |0 -> begin
          (* case (A)/(B) from comment above, just use contact_uid_in_file since its a new record *)
          let insstmt = db#stmt "peopleinsnullcid" "insert into people values(NULL,?,?,?)" in
          insstmt#bind3 service_name service_id contact_id_in_file; 
          let _ = insstmt#step_once in
          stmt#bind2 service_name service_id;
          let _ = stmt#step_once in
          stmt#column 0
       end
       |_ -> begin
          (* case (C) from comment above, there is an existing record *)
          let people_id = stmt#column 0 in
          let contact_id_in_db = stmt#column 1 in
          let () = match contact_id_in_db, contact_id_in_file with
          |Sqlite3.Data.NULL,Sqlite3.Data.NULL ->
             (*  print_endline "NULL,NULL"; *)
              () (* case (D) in comment above, but noop*)
          |Sqlite3.Data.INT contact_id_in_db, Sqlite3.Data.NULL -> 
              (* case (E) in comment above, update file *)
             (*  print_endline (sprintf "CHANGE FILE: fname=%s dbid=%Ld" fname contact_id_in_db); *)
              ()
          |Sqlite3.Data.INT contact_id_in_db, Sqlite3.Data.INT contact_id_in_file ->
              (* case (F) in comment above, check they are the same *)
              if contact_id_in_db = contact_id_in_file then begin
                (* print_endline (sprintf "EQ %Ld" contact_id_in_db); *)
                () (* no action, case (H) above *)
              end else begin
                 print_endline "CONFLICT!!!!"
              end
          |_ -> failwith "unexpected rets in contact ids"
          in
          people_id
       end
    in
    (* check if this lifedb entry already exists *)
    let stmt = db#stmt "get_lifedb" "select id from lifedb where filename=?" in
    stmt#bind1 (Sqlite3.Data.TEXT fname);
    let people_id = process_addr from_addr in
    let lifedb_id = match stmt#step_once with
    |0 ->
        (* brand new lifedb record *)
        let insstmt = db#stmt "ins_lifedb" "insert into lifedb values(NULL,?,datetime(?,\"unixepoch\"),?,?,?)" in
        let summary = Sqlite3.Data.TEXT (summaryofmsg le) in
        insstmt#bind [| (Sqlite3.Data.TEXT fname); ctime; (Sqlite3.Data.INT mtype); people_id; summary |];
        let _ = insstmt#step_once in
        stmt#bind1 (Sqlite3.Data.TEXT fname);
        let _ = stmt#step_once in
        stmt#column 0;
        (* update abrecord in people also *)
    |_ ->
        let lifedb_id = stmt#column 0 in
        print_endline "update";
        let stmt = db#stmt "up_lifedb" "update lifedb set ctime=?,mtype=?,people_id=? where id=?" in
        stmt#bind4 ctime (Sqlite3.Data.INT mtype) people_id lifedb_id;
        let _ = stmt#step_once in
        people_id
        (* XXX update abrecord in people also *)
    in 
    (* process the to entries *)
    match le._to with
    |Some addrs ->
      List.iter (fun addr ->
        let people_id = process_addr addr in
        let stmt = db#stmt "lifedb_to_ins" "insert or ignore into lifedb_to values(?,?)" in
        stmt#bind2 lifedb_id people_id;
        let _ = stmt#step_once in ()
      ) addrs 
    |None -> ()

let process_directory db rootdir dir =
  let dh = opendir dir in
  let counter = ref 0 in
  begin
  try while true do
     incr counter;
     if !counter mod 100 == 0 then printf ".%!";
     let h = readdir dh in
     if Filename.check_suffix h ".lifeentry" then begin
        let fname = sprintf "%s/%s" dir h in
        try
           process_lifeentry db rootdir fname
        with
           e -> print_endline (sprintf "exception in handling %s: %s" fname (Printexc.to_string e))
     end
  done with End_of_file -> ()
  end;
  closedir dh
  
let dir_needs_update db dir =
  let dir_mtime = Int64.of_float (Unix.stat dir).st_mtime in
  let needs_update = ref (Some dir_mtime) in
  let stmt = db#stmt "dircache" "select mtime from dircache where dir=?" in
  stmt#bind1 (Sqlite3.Data.TEXT dir);
  let () = match stmt#step_once with
  |0 -> ()
  |1 ->
    let db_mtime = Int64.of_string (Sqlite3.Data.to_string (stmt#column 0)) in
    if dir_mtime <= db_mtime then needs_update := None
  |_ -> failwith "dup dircache entries" in
  !needs_update

let dir_is_updated db dir mtime =
  let stmt = db#stmt "dircache_updated" "insert or replace into dircache values(?,?)" in
  stmt#bind [| (Sqlite3.Data.TEXT dir); (Sqlite3.Data.INT mtime) |];
  let _ = stmt#step_once in ()

let check_directory db rootdir dir = 
  printf "%s ...%!" dir;
  match dir_needs_update db dir with
  |Some old_mtime  ->
     db#transaction (fun () ->
         process_directory db rootdir dir;
         dir_is_updated db dir old_mtime;
     );
     print_endline "o"
  |None ->
     print_endline "*"

let init_db dbname =
  let db = new Sql_access.db dbname in
  db#exec "create table if not exists
       dircache (dir text primary key, mtime integer)";
  db#exec "create table if not exists
       lifedb (id integer primary key autoincrement, filename text, ctime integer, mtype integer, people_id integer, summary text)";
  db#exec "create unique index if not exists lifedb_filename on lifedb(filename)";
  db#exec "create table if not exists
       lifedb_to (lifedb_id integer, people_id integer, primary key(lifedb_id, people_id))";
  db#exec "create table if not exists
       contacts (id integer primary key autoincrement, file_name text, uid text, abrecord text, first_name text, last_name text)";
  db#exec "create unique index if not exists contacts_uid on contacts(uid)";
  db#exec "create unique index if not exists contacts_filename on contacts(file_name)";
  db#exec "create table if not exists
       people (id integer primary key autoincrement, service_name text, service_id text, contact_id integer)";
  db#exec "create unique index if not exists people_svcid on people(service_name, service_id)";
  db#register_map;
  db

let do_mirror lifedb_path db =
  walk_directory_tree lifedb_path (check_directory db lifedb_path)

let dispatch cgi args =
  ()

let singleton path =
    let hooks = object
        inherit Netplex_kit.empty_processor_hooks() as super

        val mutable signal_stop = true
        val mutable loop_delay_time = 10.
        val mutable lifedb_path = path

        method post_start_hook c =
            super#post_start_hook c;
            let dbname = Filename.concat lifedb_path "life.db" in
            let db = init_db dbname in
            ignore(Thread.create (fun () ->
                while signal_stop do
                    c#log `Info (sprintf "Initiating a sync");
                    do_mirror lifedb_path db;
                    Thread.delay loop_delay_time;
                done;
                c#log `Debug "Terminating session cleanup thread"
            ) ())

        method receive_admin_message c msg args =
            c#log `Info (sprintf "received admin msg %s %s" msg (String.concat "," (Array.to_list args)));
            match msg,args with
            |_ -> ()

        method shutdown () =
            signal_stop <- false;
            super#shutdown ()
    end in

    object (self)
        method name = "sql_mirror"
        method create_processor _ _ _ =
            object (self)
            inherit Netplex_kit.processor_base hooks
            method process ~when_done _ _ _ = when_done () (* should not ever be called *)
            method supported_ptypes = [ `Multi_threading ]
        end
    end

