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
    text ~flags:[`Unique; `Index] "file_name";
    text "mime_type";
  ], [ [], ["file_name"] ];

  "contact" , [
    text "file_name";
    text ~flags:[`Unique; `Index] "uid";
    text ~flags:[`Optional] "first_name";
    text ~flags:[`Optional] "last_name";
    date "mtime";
  ], [];

  "mtype" , [
    text ~flags:[`Unique; `Index] "name";
    text "label";
    text ~flags:[`Optional] "icon";
    text "implements";
  ], [ [],["name"] ];

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
    date ~flags:[`Index] "created";
    foreign "mtype" "mtype";
    foreign "service" "from";
    foreign_many "service" "recipients";
    foreign_many "attachment" "atts";
    foreign_many "tag" "tags";
    text ~flags:[`Optional; `Index] "inbox";
    integer "delivered";
  ], [
    ["uid"],[];
    ["created"],[];
    [],["inbox";"delivered"];
    [],["uid"];
    [],["file_name"]
  ];
]

let sync = make [
  "dircache", [
    text "dir";
    date "mtime";
  ],[];

  "filter_rule", [
    text "name";
    text "body";
    integer "zorder";
  ],[];

  "user", [
    text ~flags:[`Unique; `Index] "uid";
    text "ip";
    integer "port";
    text "key";
    date "last_sync";
    blob "has_guids";
    blob "sent_guids";
    foreign_many "filter_rule" "filters";
  ], [];

]  

let _ = 
    Sql_orm.generate ~debug:true lifedb "lifedb_schema";
    Sql_orm.generate ~debug:false sync "sync_schema";
    ()
