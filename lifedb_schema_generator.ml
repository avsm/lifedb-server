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

open Sql_orm.Schema

let lifedb = make [
  "attachment" , [
    text "file_name";
    text "mime_type";
  ], [];

  "contact" , [
    text "file_name";
    text ~flags:[`Unique; `Index] "uid";
    text ~flags:[`Optional] "first_name";
    text ~flags:[`Optional] "last_name";
    date "mtime";
  ], [];

  "mtype" , [
    text "name";
    text "label";
    text ~flags:[`Optional] "icon";
    text "implements";
  ], [];

  "service" , [
    text "name";
    text "uid";
    foreign ~flags:[`Optional] "contact" "contact";
  ],[];

  "tag" , [
    text "name"
  ], [];

  "entry" , [
    text ~flags:[`Unique; `Index] "uid";
    text "file_name";
    date "created";
    foreign "mtype" "mtype";
    foreign "service" "from";
    foreign_many "service" "recipients";
    foreign_many "attachment" "atts";
    foreign_many "tag" "tags";
    text ~flags:[`Optional] "inbox";
  ], [ ["uid"],[] ];
]

let sync = make [
  "dircache", [
    text "dir";
    date "mtime";
  ],[];

  "guid", [
    text "guid"
  ],[];

  "filter_rule", [
    text "name";
    text "body";
    integer "zorder";
  ],[];

  "user", [
    text "uid";
    text "ip";
    integer "port";
    text "key";
    date "last_sync";
    foreign_many "guid" "has_guids";
    foreign_many "guid" "sent_guids";
    foreign_many "filter_rule" "filters";
  ], [];
]

let _ = 
    Sql_orm.generate ~debug:false lifedb "lifedb_schema";
    Sql_orm.generate ~debug:false sync "sync_schema";
    ()
