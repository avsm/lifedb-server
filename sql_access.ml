open Sqlite3
open Printf

exception Sql_error of (Rc.t * string)

let raise_sql_error x =
    raise (Sql_error (x, (Rc.to_string x)))

let try_finally fn finalfn =
    try
      let r = fn () in
      finalfn ();
      r
    with e -> begin
      print_endline "WARNING: exception";
      finalfn ();
      raise e
    end

(* retry until a non-BUSY error code is returned *)
let rec db_busy_retry fn =
    match fn () with
    |Rc.BUSY -> 
       print_endline "WARNING: busy";
       Thread.delay (Random.float 1.);
       db_busy_retry fn
    |x -> x

let db_must_ok fn =
    match db_busy_retry fn with
    |Rc.OK -> ()
    |x -> raise_sql_error x

class statement db uid sql = object
    val s = prepare db sql

    method bind =
        db_must_ok (fun () -> reset s);
        Array.iteri (fun i v -> 
            db_must_ok (fun () -> bind s (i+1) v)
        )

    method bind1 arg =
        db_must_ok (fun () -> reset s);
        db_must_ok (fun () -> bind s 1 arg)

    method bind2 arg1 arg2 =
        db_must_ok (fun () -> reset s);
        db_must_ok (fun () -> bind s 1 arg1);
        db_must_ok (fun () -> bind s 2 arg2)

    method bind3 arg1 arg2 arg3 =
        db_must_ok (fun () -> reset s);
        db_must_ok (fun () -> bind s 1 arg1);
        db_must_ok (fun () -> bind s 2 arg2);
        db_must_ok (fun () -> bind s 3 arg3)

    method bind4 arg1 arg2 arg3 arg4 =
        db_must_ok (fun () -> reset s);
        db_must_ok (fun () -> bind s 1 arg1);
        db_must_ok (fun () -> bind s 2 arg2);
        db_must_ok (fun () -> bind s 3 arg3);
        db_must_ok (fun () -> bind s 3 arg4)

    method step_once =
        let () = match db_busy_retry (fun () -> step s) with
        |Rc.ROW -> ()
        |Rc.DONE -> ()
        |x -> raise_sql_error x in
        data_count s

    method column c = column s c
end

class db fname =
    let db = db_open fname in
    let stmts = Hashtbl.create 1 in
    let maps  = Hashtbl.create 1 in 

    object(self)

    method db = db

    method exec sql = 
        db_must_ok (fun () -> exec db sql)

    method stmt (uid:string) (sql:string) =
        try Hashtbl.find stmts uid with 
        |Not_found -> begin
            let s = new statement db uid sql in
            Hashtbl.add stmts uid s;
            s
        end

    method transaction fn =
        self#exec "begin";
        try
            let () = fn () in
            self#exec "end"
        with e -> begin
            print_endline "EXCEPTION IN TRANSACTION!";
            self#exec "rollback";
            raise e
        end

    method register_map = 
        let uid = "lifedb_mtype_map" in
        self#exec "create table if not exists mtype_map (id integer primary key autoincrement, mtype text, label text)";
        let string_get_stmt = self#stmt (uid^"_get") "select id from mtype_map where mtype=?" in
        let string_put_stmt = self#stmt (uid^"_put") "insert into mtype_map values(NULL,?,?)" in
        let c : (string,int64) Hashtbl.t = Hashtbl.create 1 in
        Hashtbl.add maps "mtype" (c,string_get_stmt,string_put_stmt)

    method lookup_mapping (key:string) labelfn =
        let c,string_get_stmt,string_put_stmt = Hashtbl.find maps "mtype" in
        try
           Hashtbl.find c key
        with Not_found -> begin
            string_get_stmt#bind1 (Data.TEXT key);
            match string_get_stmt#step_once with
            |0 ->
               string_put_stmt#bind2 (Data.TEXT key) (Data.TEXT (labelfn key));
               let _ = string_put_stmt#step_once in
               self#lookup_mapping key labelfn
            |_ ->
               let res = match string_get_stmt#column 0 with
               |Data.INT i -> i
               |x -> Int64.of_string (Data.to_string x) in
               Hashtbl.add c key res;
               res
        end
       
end
