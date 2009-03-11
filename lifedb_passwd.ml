open Printf
open Utils
open Sqlite3

let passphrase = ref ""

let init db =
    db#exec "create if not exists passwd (service text, ctime integer,
       username text, encpasswd text, primary key (service, username))"

let with_db fn =
    let dbname = Lifedb_config.Dir.passwd_db () in
    let db = new Sql_access.db dbname in
    init db;
    fn db

let store_passwd time service username passwd =
    let service' = Data.TEXT service in
    let time' = Data.INT time in
    let username' = Data.TEXT username in
    match Passwords.encrypt_password time !passphrase passwd with
    |Some encpasswd ->
        let encpasswd' = Data.TEXT encpasswd in
        with_db (fun db ->
           let stmt = db#stmt "checkpass" "select service,username from passwd where service=? and username=?" in
           stmt#bind2 service' username';
           match stmt#step_once with
           |0 ->
               let stmt = db#stmt "inspass" "insert into passwd values(?,?,?,?)" in
               stmt#bind4 service' time' username' encpasswd';
               let _ = stmt#step_once in ()
           |_ ->
               let stmt = db#stmt "uppass" "update passwd set ctime=?,encpasswd=? where service=? and username=?" in
               stmt#bind4 time' encpasswd' service' username'; 
               let _ = stmt#step_once in ()
        );
        true
    |None -> false

let get_passwd service username =
    let service' = Data.TEXT service in
    let username' = Data.TEXT username in
    with_db (fun db ->
      let stmt = db#stmt "getpass" "select ctime,encpasswd from passwd where service=? and username=?" in 
      stmt#bind2 service' username';
      match stmt#step_once with 
      |0 -> None
      |_ -> 
        let time' = stmt#column 0 in
        let encpasswd' = stmt#column 1 in
        let time = match time' with |Data.INT x -> x |x -> Int64.of_string (Data.to_string x) in
        let encpasswd = Data.to_string encpasswd' in
        Passwords.decrypt_password time !passphrase encpasswd
    )  
