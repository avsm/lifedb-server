(* Copyright (C) 2009 Anil Madhavapeddy <anil@recoil.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

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

let dispatch lifedb syncdb env (cgi:Netcgi.cgi_activation) = function
  |`Date bits -> begin
     let intn n = int_of_string (List.nth bits n) in
     match List.length bits with
     |3 -> begin (* specific day to query doc_ids *)
        let day,month,year = try
            intn 2, intn 1, intn 0
          with _ -> raise (Lifedb_rpc.Invalid_rpc "unknown date path")
        in 
        let btm = Unix.gmtime 0. in
        let datefrom = {btm with Unix.tm_year=(year-1900); tm_mon=month-1; tm_mday=day} in
        let dateto = {datefrom with Unix.tm_hour=23; tm_min=59; tm_sec=59} in
        let sqldate x = Sqlite3.Data.INT (Int64.of_float (fst (Unix.handle_unix_error Unix.mktime x))) in
        let sqlfrom = sqldate datefrom in
        let sqlto = sqldate dateto in
        let custom_where = "entry.created >= ? AND entry.created < ?", [sqlfrom; sqlto] in
        (* determine if we need a full query or just the ids, based on the query string *)
        match cgi#argument_value ~default:"0" "full" = "1" with
        |true-> (* return a Rpc.Entry.ts query set with full details of each entry *)
          let es = List.map (fun fname ->
             Entry.t_of_json (Json_io.load_json ~big_int_mode:true fname)
          ) (LS.Entry.get_file_name ~custom_where lifedb) in
          let r = object method results=List.length es  method rows=es end in
          Lifedb_rpc.return_json cgi (Entry.json_of_ts r)
        |false ->
          let ids = LS.Entry.get_uid ~custom_where lifedb in
          let d,_ = Unix.mktime datefrom in
          let r = object method date=d method ids=ids end in
          Lifedb_rpc.return_json cgi (Query.json_of_day_list r)
     end
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
  |`Doc id -> begin
     match LS.Entry.get_by_uid id lifedb with
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
   end
  |`Att uid -> begin
    match LS.Attachment.get_by_uid uid lifedb with
    |[] -> Lifedb_rpc.return_error cgi `Not_found "att not found" "id invalid"
    |[e] -> Lifedb_rpc.return_file cgi e#file_name e#mime_type
    |_ -> raise (Lifedb_rpc.Resource_conflict "multiple ids")
  end
  |`Contact_query (mode,tl) -> begin
    match mode with
    |"date" -> begin
      let btm = Unix.gmtime 0. in
      let fulldate = List.map (fun n -> try Some (int_of_string n) with _ -> None) tl in
      match fulldate with
      |[(Some year); (Some month); (Some day)] ->
        let datefrom = {btm with Unix.tm_year=(year-1900); tm_mon=month-1; tm_mday=day} in
        let dateto = {datefrom with Unix.tm_hour=23; tm_min=59; tm_sec=59} in
        let sqldate x = Sqlite3.Data.INT (Int64.of_float (fst (Unix.handle_unix_error Unix.mktime x))) in
        let sqlfrom = sqldate datefrom in
        let sqlto = sqldate dateto in
        let custom_where = "entry.created >= ? AND entry.created < ?", [sqlfrom; sqlto] in
        let qs = LS.Entry.get_from_recipients ~custom_where lifedb in
        let contact_of_res svc = match svc#contact with
          |Some c -> Some (object 
            method first_name=c#first_name 
            method last_name=c#last_name 
            method uid=c#uid
          end )
          |None -> None
        in
        let contacts = List.fold_left (fun a (frm,recip) ->
          let r = (contact_of_res frm) :: (List.map contact_of_res recip) in
          let r = List.fold_left (fun a -> function None -> a |Some b -> b :: a) [] r in
          r @ a
        ) [] qs in 
        let contacts = results_of_search (unique (fun x y -> x#uid <> y#uid) contacts) in
        Lifedb_rpc.return_json cgi (Query.json_of_contacts contacts)
      
      |_ -> Lifedb_rpc.return_error cgi `Not_found "bad query date" "need yr/<month>/<day>"
    end
    |_ -> Lifedb_rpc.return_error cgi `Not_found "bad mode" "bad mode"
  end
