open Printf
open Lifedb.Rpc

let with_db fn =
     let db = new Sql_access.db (Lifedb_config.Dir.lifedb_db ()) in
     fn db

let dispatch cgi = function
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
        let sql = sprintf
           "SELECT lifedb.id FROM lifedb WHERE
            ctime >= datetime(?, 'unixepoch') AND ctime < datetime(?, 'unixepoch') 
            ORDER BY ctime DESC" in
        let stmt = with_db (fun db -> db#stmt "getmsgs" sql) in
        stmt#bind2 sqlfrom sqlto;
        let ids = ref [] in
        stmt#step_all (fun () ->
            let id = stmt#int_col 0 in
            ids := (Int64.to_string id) :: !ids;
        );
        let d,_ = Unix.mktime datefrom in
        let r = object method date=d method ids=(!ids) end in
        cgi#output#output_string (Json_io.string_of_json (Query.json_of_day_list r))
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
        let sql = "SELECT strftime('%d', ctime) FROM lifedb WHERE ctime >= datetime(?, 'unixepoch') AND ctime < datetime(?,'unixepoch')" in
        let stmt = with_db (fun db -> db#stmt "getdaycount" sql) in
        stmt#bind2 sqlfrom sqlto;
        let freq = Array.create 31 0 in
        stmt#step_all (fun () ->
           let day = Int64.to_int (stmt#int_col 0) in
           freq.(day-1) <- freq.(day-1) + 1
        );
        let r = object method year=year method month=month method days=freq end in
        cgi#output#output_string (Json_io.string_of_json (Query.json_of_month_list r));
     |_ ->
        Lifedb_rpc.return_error cgi `Not_found "bad date" "unknown date format"
   end
  |`Doc id ->
     let sql = "select filename from lifedb where id=?" in
     let stmt = with_db (fun db -> db#stmt "getdoc" sql) in
     stmt#bind1 (Sqlite3.Data.INT (Int64.of_string id));
     match stmt#step_once with
     |0 -> Lifedb_rpc.return_error cgi `Not_found "doc not found" "id invalid"
     |_ -> 
       let fname = stmt#str_col 0 in
       let fh = open_in_bin fname in
       let size = in_channel_length fh in
       let user_filename = Pcre.qreplace ~rex:(Pcre.regexp "[ \\\"\\\\]") ~templ:"_" (Filename.basename fname) in
       cgi#set_header ~content_type:"application/octet-stream" ~content_length:size ~cache:`No_cache ~filename:user_filename ();
       let ch = new Netchannels.input_channel fh in
       cgi#output#output_channel ch;
       ch#close_in()
