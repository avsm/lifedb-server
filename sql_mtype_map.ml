(* registering mtypes is a pretty rare operation, so just open/close the
   db for this purpose within the calling thread *)
open Utils
open Sqlite3
open Printf

let update db params =
    let mtype = Data.TEXT params#pltype in
    let descr = Data.TEXT params#description in
    let implements = Data.TEXT params#implements in
    let icon = match params#icon with |None -> Data.NULL |Some t -> Data.TEXT t in
    db#exec "create table if not exists mtype_map (id integer primary key autoincrement, mtype text, 
        label text, icon text, implements text)";
    let gets = db#stmt "update_mtype_get" "select id from mtype_map where mtype=?" in
    db#transaction (fun () ->
        gets#bind1 mtype;
        match gets#step_once with
        |0 -> (* insert a new mtype row *)
           let puts = db#stmt "update_mtype_put" "insert into mtype_map values(NULL,?,?,?,?)" in
           print_endline (sprintf "inserting new mtype %s" params#pltype);
           puts#bind [| mtype; descr; icon; implements |];
           let _ = puts#step_once in ()
        |_ -> (* update the existing row *)
           let upds = db#stmt "update_mtype_upd" "update mtype_map set label=?,icon=?,implements=? where id=?" in
           print_endline (sprintf "updating existing mtype %s" params#pltype);
           let id = gets#column 0 in
           print_endline (sprintf "id=%s" (Data.to_string id));
           upds#bind [| descr; icon; implements; id |];
           print_endline "bound upds";
           let _ = upds#step_once in
           print_endline "done updas";
    ) 
