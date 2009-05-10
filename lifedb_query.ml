open Printf
open Lifedb.Rpc
open Utils

module LS=Lifedb_schema
module SS=Sync_schema

let decl_of_db_mtype m =
  object
    method pltype=m#name
    method description=m#label
    method implements=m#implements
    method icon=m#icon
  end

let dispatch lifedb syncdb env cgi = function
  |`Date bits -> begin
     let intn n = int_of_string (List.nth bits n) in
     match List.length bits with
     |3 -> (* specific day to query doc_ids *)
        let day,month,year = try
            intn 2, intn 1, intn 0
          with _ -> raise (Lifedb_rpc.Invalid_rpc "unknown date path")
        in 
        let btm = Unix.gmtime 0. in
        let datefrom = {btm with Unix.tm_year=(year-1900); tm_mon=month; tm_mday=day} in
        let dateto = {datefrom with Unix.tm_hour=23; tm_min=59; tm_sec=59} in
        let sqldate x = Sqlite3.Data.INT (Int64.of_float (fst (Unix.handle_unix_error Unix.mktime x))) in
        let sqlfrom = sqldate datefrom in
        let sqlto = sqldate dateto in
        let ids = LS.Entry.get_uid ~custom_where:("entry.created >= ? AND entry.created < ?", [sqlfrom; sqlto]) lifedb in
        let d,_ = Unix.mktime datefrom in
        let r = object method date=d method ids=ids end in
        Lifedb_rpc.return_json cgi (Query.json_of_day_list r)
     |2 -> (* query the number of docs per day in a month *)
        let month,year = try intn 1, intn 0 with _ -> raise (Lifedb_rpc.Invalid_rpc "unknown date path") in
        let btm = Unix.gmtime 0. in
        let datefrom = {btm with Unix.tm_year=(year-1900); tm_mon=month-1; tm_mday=1} in
        let nextyear, nextmonth = match year,month with
        |yr,12 -> yr+1,1
        |yr,mn -> yr,mn+1 in 
        let dateto = {datefrom with Unix.tm_hour=0; tm_min=0; tm_sec=0; tm_year=(nextyear-1900); tm_mon=nextmonth-1; tm_mday=1} in
        let sqldate x = Sqlite3.Data.INT (Int64.of_float (fst (Unix.handle_unix_error Unix.mktime x))) in
        let sqlfrom = sqldate datefrom in
        let sqlto = sqldate dateto in
        let created = LS.Entry.get_created ~custom_where:("entry.created >= ? AND entry.created < ?", [sqlfrom; sqlto]) lifedb in
        let freq = Array.create 31 0 in
        List.iter (fun c ->
           let day = (Unix.gmtime c).Unix.tm_mday in
           freq.(day-1) <- freq.(day-1) + 1
        ) created;
        let r = object method year=year method month=month method days=freq end in
        Lifedb_rpc.return_json cgi (Query.json_of_month_list r)
     |_ ->
        Lifedb_rpc.return_error cgi `Not_found "bad date" "unknown date format"
   end
  |`Mtype bits -> begin
     match bits with
     |[] -> begin (* list of known mtypes *)
        let mtypes = LS.Mtype.get lifedb in
        let decls = List.map decl_of_db_mtype mtypes in
        Lifedb_rpc.return_json cgi (Plugin.json_of_decls decls)
     end
     |mtype :: tl -> begin (* info on mtype *)
       match LS.Mtype.get_by_name mtype lifedb with
       |[] -> Lifedb_rpc.return_error cgi `Not_found "pltype not found" "unknown pltype"
       |[m] -> begin
         match tl with
         |[opt] when opt = "icon" -> begin
           match m#icon with
           |None -> Lifedb_rpc.return_error cgi `Not_found "No icon" "This plugin doesnt have an icon registered"
           |Some icon -> Lifedb_rpc.return_file cgi icon "image/png"
         end
         |_ ->
           Lifedb_rpc.return_json cgi (Plugin.json_of_decl (decl_of_db_mtype m))
       end
       |_ -> raise (Lifedb_rpc.Resource_conflict "multiple pltypes")
     end
   end
  |`Doc id ->
     match LS.Entry.get ~id:(Some (Int64.of_string id)) lifedb with
     |[] -> Lifedb_rpc.return_error cgi `Not_found "doc not found" "id invalid"
     |[e] ->  begin
       let json = Entry.t_of_json (Json_io.load_json ~big_int_mode:true e#file_name) in
       let services = e#from :: e#recipients in
       let chash = Hashtbl.create 1 in
           List.iter (fun s ->
             match s#contact with 
             |None -> ()
             |Some c ->
               let js = object
                 method id = Int64.to_string (match c#id with |None -> -1L |Some x -> x)
                 method uid = c#uid
                 method first_name = c#first_name
                 method last_name = c#last_name
               end in
               let svch = try Hashtbl.find chash s#name with Not_found -> let h=Hashtbl.create 1 in Hashtbl.add chash s#name h; h in
               Hashtbl.replace svch s#uid js
           ) services;
       let r = object method entry=json method contacts=chash end in
       Lifedb_rpc.return_json cgi (Entry.json_of_doc r)
     end
     |_ -> raise (Lifedb_rpc.Resource_conflict "multiple ids")
