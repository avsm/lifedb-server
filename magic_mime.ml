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

let initialized = ref false
let magic = Hashtbl.create 1

let mime_types = ref []

let read_magic_mime () =
  let fin = open_in "mime.types" in
  let split = Netstring_pcre.split (Pcre.regexp "[ \r\t\n]+") in
  try
    while true do
      let l = input_line fin in
      if l = "" || l.[0] <> '#' then (
        match split l with
          |[mime_type] -> ()
          |[] -> ()
          |mime_type :: exts ->
            let m = String.lowercase mime_type in
            List.iter (fun ext -> Hashtbl.replace magic ext m) exts;
            mime_types := mime_type :: !mime_types
      )
    done; ()
  with
  |End_of_file -> close_in fin
  |e -> close_in fin; raise e

let lookup ext =
  if not !initialized then
    read_magic_mime ();
  try
    Hashtbl.find magic (String.lowercase ext)
  with
    |Not_found -> "application/octet-stream"

let all_mime_types () =
  if not !initialized then
    read_magic_mime ();
  !mime_types
