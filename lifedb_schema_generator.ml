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
    text ~flags:[`Unique; `Index] "uid";
    text "mime_type";
  ], [ [], ["file_name"]; [], ["uid"] ], default_opts;

  "contact" , [
    text "file_name";
    text ~flags:[`Unique; `Index] "uid";
    text ~flags:[`Optional] "first_name";
    text ~flags:[`Optional] "last_name";
    date "mtime";
  ], [], default_opts;

  "mtype" , [
    text ~flags:[`Unique; `Index] "name";
    text "label";
    text ~flags:[`Optional] "icon";
    text "implements";
  ], [ [],["name"] ], default_opts;

  "service" , [
    text "name";
    text "uid";
    foreign ~flags:[`Optional] "contact" "contact";
  ],[], default_opts;

  "tag" , [
    text "name"
  ], [], default_opts;

  "entry" , [
    text ~flags:[`Unique; `Index] "uid";
    text "file_name";
    date ~flags:[`Index] "created";
    foreign "mtype" "mtype";
    foreign "service" "from";
    foreign_many "service" "recipients";
    foreign_many "attachment" "atts";
    foreign_many "tag" "tags";
    integer "delivered";
  ], [
    ["uid"],[];
    ["file_name"],[];
    ["created"],[];
    ["from";"recipients"],[];
    [],["uid"];
    [],["file_name"]
  ], default_opts;
]

let sync = make [
  "dircache", [
    text "dir";
    date "mtime";
  ],[], default_opts;

]  

let log = make [
  "task", [
     text "name";
     date "started";
     real "time_taken";
     integer "exit_code";
   ], [], default_opts;
]

let keychain = make [
  "passwd", [
     text "service";
     date "ctime";
     text "username";
     text "encpasswd";
     text "comment";
   ], [
     [], ["service";"username"] 
   ], { unique = [ [ "service"; "username" ] ] }
]

let _ = 
    Sql_orm.generate ~debug:false lifedb "lifedb_schema";
    Sql_orm.generate ~debug:false sync "sync_schema";
    Sql_orm.generate ~debug:false log "log_schema";
    Sql_orm.generate ~debug:false keychain "keychain_schema";
    ()
