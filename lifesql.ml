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

(* cmd line wrapper to use the SQL mirror from outside daemon *)

let _ =
  Lifedb_config.read_config "config.json";
  Log.init ();
  let lifedb = Lifedb_schema.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = Sync_schema.Init.t (Lifedb_config.Dir.sync_db ()) in
  try
    Sql_mirror.do_scan lifedb syncdb;
  with e -> (
    Thread.delay 2.;
    raise e
  )

