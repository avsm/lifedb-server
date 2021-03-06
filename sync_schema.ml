(* autogenerated by sql_orm *)
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
      raise (Sqlite3.Error (Rc.to_string x))
  
  let try_finally fn finalfn =
      try
        let r = fn () in
        finalfn ();
        r
      with e -> begin
        print_endline (sprintf "WARNING: exception: %s" (Printexc.to_string e));
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
  
  (* make sure a DONE is returned from the database *)
  let db_must_done db fn = 
     match db_busy_retry db fn with
     |Rc.DONE -> ()
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
    let sql = "create table if not exists dircache (id integer primary key autoincrement,dir text,mtime real);" in
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
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _mtime in Sqlite3.Data.FLOAT v));
        db_must_done db (fun () -> Sqlite3.step stmt);
        let __id = Sqlite3.last_insert_rowid db.db in
        _id <- Some __id;
        __id
      |Some id -> (* update *)
        let sql = "UPDATE dircache SET dir=?,mtime=? WHERE id=?" in
        let stmt = Sqlite3.prepare db.db sql in
        db_must_ok db (fun () -> Sqlite3.bind stmt 1 (let v = _dir in Sqlite3.Data.TEXT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 2 (let v = _mtime in Sqlite3.Data.FLOAT v));
        db_must_ok db (fun () -> Sqlite3.bind stmt 3 (Sqlite3.Data.INT id));
        db_must_done db (fun () -> Sqlite3.step stmt);
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
      db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (Sqlite3.Data.FLOAT v));
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
        |x -> match x with |Sqlite3.Data.FLOAT i -> i|x -> (try float_of_string (Sqlite3.Data.to_string x) with _ -> failwith "error: dircache mtime"))
      )
      (* foreign fields *)
    db
    in 
    (* execute the SQL query *)
    step_fold db stmt of_stmt

end

module Init = struct
  type t = state
  type transaction_mode = [`Exclusive |`Deferred |`Immediate ]
  let t ?(busyfn=default_busyfn) ?(mode=`Immediate) db_name =
    let db = {db=Sqlite3.db_open db_name; in_transaction=0; mode=mode; busyfn=busyfn } in
    Dircache.init db;
    db

  let db handle = handle.db
end

