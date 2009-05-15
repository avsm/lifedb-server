(* autogenerated by sql_orm *)
exception Sql_error of (Sqlite3.Rc.t * string)
module Sql_access = struct
  (*
   * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
   *
   * Permission to use, copy, modify, and distribute this software for any
   * purpose with or without fee is hereby granted, provided that the above
   * copyright notice and this permission notice appear in all copies.
   *
   * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
   *)
  
  open Sqlite3
  open Printf
  
  type transaction_mode = [
      |`Deferred
      |`Immediate
      |`Exclusive
  ]
  
  type state = {
      db : db;
      mutable in_transaction: int;
      busyfn: db -> unit;
      mode: transaction_mode;
  }
  
  let default_busyfn (db:Sqlite3.db) =
      print_endline "WARNING: busy";
      Thread.delay (Random.float 1.)
  
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
  let rec db_busy_retry db fn =
      match fn () with
      |Rc.BUSY -> 
         db.busyfn db.db;
         db_busy_retry db fn;
      |x -> x
  
  (* make sure an OK is returned from the database *)
  let db_must_ok db fn =
      match db_busy_retry db fn with
      |Rc.OK -> ()
      |x -> raise_sql_error x
  
  (* request a transaction *)
  let transaction db fn =
      let m = match db.mode with
      |`Deferred -> "DEFERRED" |`Immediate -> "IMMEDIATE" |`Exclusive -> "EXCLUSIVE" in
      try_finally (fun () ->
          if db.in_transaction = 0 then (
             db_must_ok db (fun () -> exec db.db (sprintf "BEGIN %s TRANSACTION" m));
          );
          db.in_transaction <- db.in_transaction + 1;
          fn ();
      ) (fun () ->
          if db.in_transaction = 1 then (
             db_must_ok db (fun () -> exec db.db "END TRANSACTION");
          );
          db.in_transaction <- db.in_transaction - 1
      )
  
  (* iterate over a result set *)
  let step_fold db stmt iterfn =
      let stepfn () = Sqlite3.step stmt in
      let rec fn a = match db_busy_retry db stepfn with
      |Sqlite3.Rc.ROW -> fn (iterfn stmt :: a)
      |Sqlite3.Rc.DONE -> a
      |x -> raise_sql_error x
      in
      fn []
end


open Sql_access
module Dircache = struct
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    dir : string;
    set_dir : string -> unit;
    mtime : float;
    set_mtime : float -> unit;
    save: int64; delete: unit
  >

  let init db =
    let sql = "create table if not exists dircache (id integer primary key autoincrement,dir text,mtime integer);" in
    db_must_ok db (fun () -> Sqlite3.exec db.db sql);
    ()

  (* object definition *)
  let t ?(id=None) ~dir ~mtime db : t = object
    (* get functions *)
    val mutable _id = id
    method id : int64 option = _id
    val mutable _dir = dir
    method dir : string = _dir
    val mutable _mtime = mtime
    method mtime : float = _mtime

    (* set functions *)
    method set_id v =
      _id <- v
    method set_dir v =
      _dir <- v
    method set_mtime v =
      _mtime <- v

    (* admin functions *)
    method delete =
      match _id with
      |None -> ()
      |Some id ->
        let sql = "DELETE FROM dircache WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));
        ignore(step_fold db stmt (fun _ -> ()));
        _id <- None

    method save = transaction db (fun () ->
      (* insert any foreign-one fields into their table and get id *)
      let _curobj_id = match _id with
      |None -> (* insert new record *)
        let sql = "INSERT INTO dircache VALUES(NULL,?,?)" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _dir in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _mtime in Sqlite3.Data.INT (Int64.of_float v)));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        let __id = Sqlite3.last_insert_rowid db.db in
        _id <- Some __id;
        __id
      |Some id -> (* update *)
        let sql = "UPDATE dircache SET dir=?,mtime=? WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _dir in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _mtime in Sqlite3.Data.INT (Int64.of_float v)));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (Sqlite3.Data.INT id));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        id
      in
      _curobj_id
    )
  end

  (* General get function for any of the columns *)
  let get ?(id=None) ?(dir=None) ?(mtime=None) ?(custom_where=("",[])) db =
    (* assemble the SQL query string *)
    let q = "" in
    let _first = ref true in
    let f () = match !_first with |true -> _first := false; " WHERE " |false -> " AND " in
    let q = match id with |None -> q |Some b -> q ^ (f()) ^ "dircache.id=?" in
    let q = match dir with |None -> q |Some b -> q ^ (f()) ^ "dircache.dir=?" in
    let q = match mtime with |None -> q |Some b -> q ^ (f()) ^ "dircache.mtime=?" in
    let q = match custom_where with |"",_ -> q |w,_ -> q ^ (f()) ^ "(" ^ w ^ ")" in
    let sql="SELECT dircache.id, dircache.dir, dircache.mtime FROM dircache " ^ q in
    let stmt=Sqlite3.prepare db.db sql in
    (* bind the position variables to the statement *)
    let bindpos = ref 1 in
    ignore(match id with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT v));
      incr bindpos
    );
    ignore(match dir with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match mtime with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT (Int64.of_float v)));
      incr bindpos
    );
    ignore(match custom_where with |_,[] -> () |_,eb ->
      List.iter (fun b ->
        db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos b);
        incr bindpos
      ) eb);
    (* convert statement into an ocaml object *)
    let of_stmt stmt =
    t
      (* native fields *)
      ~id:(
      (match Sqlite3.column stmt 0 with
        |Sqlite3.Data.NULL -> None
        |x -> Some (match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: dircache id")))
      )
      ~dir:(
      (match Sqlite3.column stmt 1 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~mtime:(
      (match Sqlite3.column stmt 2 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> match x with |Sqlite3.Data.INT i -> Int64.to_float i|_ -> float_of_string (Sqlite3.Data.to_string x))
      )
      (* foreign fields *)
    db
    in 
    (* execute the SQL query *)
    step_fold db stmt of_stmt

end

module Filter_rule = struct
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    name : string;
    set_name : string -> unit;
    body : string;
    set_body : string -> unit;
    zorder : int64;
    set_zorder : int64 -> unit;
    save: int64; delete: unit
  >

  let init db =
    let sql = "create table if not exists filter_rule (id integer primary key autoincrement,name text,body text,zorder integer);" in
    db_must_ok db (fun () -> Sqlite3.exec db.db sql);
    ()

  (* object definition *)
  let t ?(id=None) ~name ~body ~zorder db : t = object
    (* get functions *)
    val mutable _id = id
    method id : int64 option = _id
    val mutable _name = name
    method name : string = _name
    val mutable _body = body
    method body : string = _body
    val mutable _zorder = zorder
    method zorder : int64 = _zorder

    (* set functions *)
    method set_id v =
      _id <- v
    method set_name v =
      _name <- v
    method set_body v =
      _body <- v
    method set_zorder v =
      _zorder <- v

    (* admin functions *)
    method delete =
      match _id with
      |None -> ()
      |Some id ->
        let sql = "DELETE FROM filter_rule WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));
        ignore(step_fold db stmt (fun _ -> ()));
        _id <- None

    method save = transaction db (fun () ->
      (* insert any foreign-one fields into their table and get id *)
      let _curobj_id = match _id with
      |None -> (* insert new record *)
        let sql = "INSERT INTO filter_rule VALUES(NULL,?,?,?)" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _name in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _body in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (let v = _zorder in Sqlite3.Data.INT v));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        let __id = Sqlite3.last_insert_rowid db.db in
        _id <- Some __id;
        __id
      |Some id -> (* update *)
        let sql = "UPDATE filter_rule SET name=?,body=?,zorder=? WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _name in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _body in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (let v = _zorder in Sqlite3.Data.INT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 4 (Sqlite3.Data.INT id));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        id
      in
      _curobj_id
    )
  end

  (* General get function for any of the columns *)
  let get ?(id=None) ?(name=None) ?(body=None) ?(zorder=None) ?(custom_where=("",[])) db =
    (* assemble the SQL query string *)
    let q = "" in
    let _first = ref true in
    let f () = match !_first with |true -> _first := false; " WHERE " |false -> " AND " in
    let q = match id with |None -> q |Some b -> q ^ (f()) ^ "filter_rule.id=?" in
    let q = match name with |None -> q |Some b -> q ^ (f()) ^ "filter_rule.name=?" in
    let q = match body with |None -> q |Some b -> q ^ (f()) ^ "filter_rule.body=?" in
    let q = match zorder with |None -> q |Some b -> q ^ (f()) ^ "filter_rule.zorder=?" in
    let q = match custom_where with |"",_ -> q |w,_ -> q ^ (f()) ^ "(" ^ w ^ ")" in
    let sql="SELECT filter_rule.id, filter_rule.name, filter_rule.body, filter_rule.zorder FROM filter_rule " ^ q in
    let stmt=Sqlite3.prepare db.db sql in
    (* bind the position variables to the statement *)
    let bindpos = ref 1 in
    ignore(match id with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT v));
      incr bindpos
    );
    ignore(match name with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match body with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match zorder with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT v));
      incr bindpos
    );
    ignore(match custom_where with |_,[] -> () |_,eb ->
      List.iter (fun b ->
        db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos b);
        incr bindpos
      ) eb);
    (* convert statement into an ocaml object *)
    let of_stmt stmt =
    t
      (* native fields *)
      ~id:(
      (match Sqlite3.column stmt 0 with
        |Sqlite3.Data.NULL -> None
        |x -> Some (match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: filter_rule id")))
      )
      ~name:(
      (match Sqlite3.column stmt 1 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~body:(
      (match Sqlite3.column stmt 2 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~zorder:(
      (match Sqlite3.column stmt 3 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: filter_rule zorder"))
      )
      (* foreign fields *)
    db
    in 
    (* execute the SQL query *)
    step_fold db stmt of_stmt

end

module User = struct
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    uid : string;
    set_uid : string -> unit;
    ip : string;
    set_ip : string -> unit;
    port : int64;
    set_port : int64 -> unit;
    key : string;
    set_key : string -> unit;
    last_sync : float;
    set_last_sync : float -> unit;
    has_guids : string;
    set_has_guids : string -> unit;
    sent_guids : string;
    set_sent_guids : string -> unit;
    filters : Filter_rule.t list;
    set_filters : Filter_rule.t list -> unit;
    save: int64; delete: unit
  >

  let init db =
    let sql = "create table if not exists user (id integer primary key autoincrement,uid text,ip text,port integer,key text,last_sync integer,has_guids blob,sent_guids blob);" in
    db_must_ok db (fun () -> Sqlite3.exec db.db sql);
    let sql = "create table if not exists map_filters_user_filter_rule (user_id integer, filter_rule_id integer, primary key(user_id, filter_rule_id));" in
    db_must_ok db (fun () -> Sqlite3.exec db.db sql);
    let sql = "CREATE UNIQUE INDEX IF NOT EXISTS user_uid_idx ON user (uid) " in
    db_must_ok db (fun () -> Sqlite3.exec db.db sql);
    ()

  (* object definition *)
  let t ?(id=None) ~uid ~ip ~port ~key ~last_sync ~has_guids ~sent_guids ~filters db : t = object
    (* get functions *)
    val mutable _id = id
    method id : int64 option = _id
    val mutable _uid = uid
    method uid : string = _uid
    val mutable _ip = ip
    method ip : string = _ip
    val mutable _port = port
    method port : int64 = _port
    val mutable _key = key
    method key : string = _key
    val mutable _last_sync = last_sync
    method last_sync : float = _last_sync
    val mutable _has_guids = has_guids
    method has_guids : string = _has_guids
    val mutable _sent_guids = sent_guids
    method sent_guids : string = _sent_guids
    val mutable _filters = filters
    method filters : Filter_rule.t list = _filters

    (* set functions *)
    method set_id v =
      _id <- v
    method set_uid v =
      _uid <- v
    method set_ip v =
      _ip <- v
    method set_port v =
      _port <- v
    method set_key v =
      _key <- v
    method set_last_sync v =
      _last_sync <- v
    method set_has_guids v =
      _has_guids <- v
    method set_sent_guids v =
      _sent_guids <- v
    method set_filters v =
      _filters <- v

    (* admin functions *)
    method delete =
      match _id with
      |None -> ()
      |Some id ->
        let sql = "DELETE FROM user WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));
        ignore(step_fold db stmt (fun _ -> ()));
        _id <- None

    method save = transaction db (fun () ->
      (* insert any foreign-one fields into their table and get id *)
      let _curobj_id = match _id with
      |None -> (* insert new record *)
        let sql = "INSERT INTO user VALUES(NULL,?,?,?,?,?,?,?)" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _uid in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _ip in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (let v = _port in Sqlite3.Data.INT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 4 (let v = _key in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 5 (let v = _last_sync in Sqlite3.Data.INT (Int64.of_float v)));
        db_must_ok db (fun () -> Sqlite3.bind stmt 6 (let v = _has_guids in Sqlite3.Data.BLOB v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 7 (let v = _sent_guids in Sqlite3.Data.BLOB v));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        let __id = Sqlite3.last_insert_rowid db.db in
        _id <- Some __id;
        __id
      |Some id -> (* update *)
        let sql = "UPDATE user SET uid=?,ip=?,port=?,key=?,last_sync=?,has_guids=?,sent_guids=? WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _uid in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _ip in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (let v = _port in Sqlite3.Data.INT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 4 (let v = _key in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 5 (let v = _last_sync in Sqlite3.Data.INT (Int64.of_float v)));
        db_must_ok db (fun () -> Sqlite3.bind stmt 6 (let v = _has_guids in Sqlite3.Data.BLOB v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 7 (let v = _sent_guids in Sqlite3.Data.BLOB v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 8 (Sqlite3.Data.INT id));
        ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)
        id
      in
      List.iter (fun f ->
        let _refobj_id = f#save in
        let sql = "INSERT OR IGNORE INTO map_filters_user_filter_rule VALUES(?,?)" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (Sqlite3.Data.INT _refobj_id));
        ignore(step_fold db stmt (fun _ -> ()));
      ) _filters;
      let ids = String.concat "," (List.map (fun x -> match x#id with |None -> assert false |Some x -> Int64.to_string x) _filters) in
      let sql = "DELETE FROM map_filters_user_filter_rule WHERE user_id=? AND (filter_rule_id NOT IN (" ^ ids ^ "))" in
      let stmt = Sqlite3.prepare db.db sql in
      db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));
      ignore(step_fold db stmt (fun _ -> ()));
      _curobj_id
    )
  end

  (* General get function for any of the columns *)
  let get ?(id=None) ?(uid=None) ?(ip=None) ?(port=None) ?(key=None) ?(last_sync=None) ?(has_guids=None) ?(sent_guids=None) ?(custom_where=("",[])) db =
    (* assemble the SQL query string *)
    let q = "" in
    let _first = ref true in
    let f () = match !_first with |true -> _first := false; " WHERE " |false -> " AND " in
    let q = match id with |None -> q |Some b -> q ^ (f()) ^ "user.id=?" in
    let q = match uid with |None -> q |Some b -> q ^ (f()) ^ "user.uid=?" in
    let q = match ip with |None -> q |Some b -> q ^ (f()) ^ "user.ip=?" in
    let q = match port with |None -> q |Some b -> q ^ (f()) ^ "user.port=?" in
    let q = match key with |None -> q |Some b -> q ^ (f()) ^ "user.key=?" in
    let q = match last_sync with |None -> q |Some b -> q ^ (f()) ^ "user.last_sync=?" in
    let q = match has_guids with |None -> q |Some b -> q ^ (f()) ^ "user.has_guids=?" in
    let q = match sent_guids with |None -> q |Some b -> q ^ (f()) ^ "user.sent_guids=?" in
    let q = match custom_where with |"",_ -> q |w,_ -> q ^ (f()) ^ "(" ^ w ^ ")" in
    let sql="SELECT user.id, user.uid, user.ip, user.port, user.key, user.last_sync, user.has_guids, user.sent_guids FROM user " ^ q in
    let stmt=Sqlite3.prepare db.db sql in
    (* bind the position variables to the statement *)
    let bindpos = ref 1 in
    ignore(match id with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT v));
      incr bindpos
    );
    ignore(match uid with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match ip with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match port with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT v));
      incr bindpos
    );
    ignore(match key with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.TEXT v));
      incr bindpos
    );
    ignore(match last_sync with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.INT (Int64.of_float v)));
      incr bindpos
    );
    ignore(match has_guids with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.BLOB v));
      incr bindpos
    );
    ignore(match sent_guids with |None -> () |Some v ->
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.BLOB v));
      incr bindpos
    );
    ignore(match custom_where with |_,[] -> () |_,eb ->
      List.iter (fun b ->
        db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos b);
        incr bindpos
      ) eb);
    (* convert statement into an ocaml object *)
    let of_stmt stmt =
    t
      (* native fields *)
      ~id:(
      (match Sqlite3.column stmt 0 with
        |Sqlite3.Data.NULL -> None
        |x -> Some (match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: user id")))
      )
      ~uid:(
      (match Sqlite3.column stmt 1 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~ip:(
      (match Sqlite3.column stmt 2 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~port:(
      (match Sqlite3.column stmt 3 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: user port"))
      )
      ~key:(
      (match Sqlite3.column stmt 4 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~last_sync:(
      (match Sqlite3.column stmt 5 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> match x with |Sqlite3.Data.INT i -> Int64.to_float i|_ -> float_of_string (Sqlite3.Data.to_string x))
      )
      ~has_guids:(
      (match Sqlite3.column stmt 6 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      ~sent_guids:(
      (match Sqlite3.column stmt 7 with
        |Sqlite3.Data.NULL -> failwith "null of_stmt"
        |x -> Sqlite3.Data.to_string x)
      )
      (* foreign fields *)
      ~filters:(
        (* foreign many-many mapping field *)
        let sql' = "select filter_rule_id from map_filters_user_filter_rule where user_id=?" in
        let stmt' = Sqlite3.prepare db.db sql' in
        let user__id = Sqlite3.column stmt 0 in
        db_must_ok db (fun () -> Sqlite3.bind stmt' 1 user__id);
        List.flatten (step_fold db stmt' (fun s ->
          let i = match Sqlite3.column s 0 with |Sqlite3.Data.INT i -> i |_ -> assert false in
          Filter_rule.get ~id:(Some i) db)
        ))
    db
    in 
    (* execute the SQL query *)
    step_fold db stmt of_stmt

end

exception Sql_error of (Sqlite3.Rc.t * string)
module Init = struct
  type t = state
  type transaction_mode = [`Exclusive |`Deferred |`Immediate ]
  let t ?(busyfn=default_busyfn) ?(mode=`Immediate) db_name =
    let db = {db=Sqlite3.db_open db_name; in_transaction=0; mode=mode; busyfn=busyfn } in
    Dircache.init db;
    Filter_rule.init db;
    User.init db;
    db

  let db handle = handle.db
end

