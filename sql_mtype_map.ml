open Utils
open Sqlite3
open Printf
open Lifedb_schema

let update db plugin_dir params =
    let name = params#pltype in
    let label = params#description in
    let implements = params#implements in
    let icon = match params#icon with
      |None -> None
      |Some t -> 
         let ft = if Filename.is_relative t then
            Filename.concat plugin_dir t
         else t in
         Some ft
    in
    let mtype = match Mtype.get ~name:(Some name) db with
    |[] -> Mtype.t ~name ~label ~implements ~icon db 
    |[m] -> m#set_label label; m#set_implements implements; m#set_icon icon; m
    |_ -> assert false in
    ignore(mtype#save)
