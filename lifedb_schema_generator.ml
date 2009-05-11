(* Copyright (C) 2009 Anil Madhavapeddy <anil@recoil.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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
