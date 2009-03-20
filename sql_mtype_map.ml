open Utils
open Sqlite3
open Printf

let update db plugin_dir params =
    let mtype = Data.TEXT params#pltype in
    let descr = Data.TEXT params#description in
    let implements = Data.TEXT params#implements in
    let icon = match params#icon with
      |None -> Data.NULL 
      |Some t -> 
         let ft = if Filename.is_relative t then
            Filename.concat plugin_dir t
         else t in
         Data.TEXT ft
    in
    let gets = db#stmt "update_mtype_get" "select id from mtype_map where mtype=?" in
    db#transaction (fun () ->
        gets#bind1 mtype;
        match gets#step_once with
        |0 -> (* insert a new mtype row *)
           let puts = db#stmt "update_mtype_put" "insert into mtype_map values(NULL,?,?,?,?)" in
           puts#bind [| mtype; descr; icon; implements |];
           let _ = puts#step_once in ()
        |_ -> (* update the existing row *)
           let upds = db#stmt "update_mtype_upd" "update mtype_map set label=?,icon=?,implements=? where id=?" in
           let id = gets#column 0 in
           upds#bind [| descr; icon; implements; id |];
           let _ = upds#step_once in ()
    )     

