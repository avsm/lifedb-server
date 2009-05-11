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

(* Platform-specific implementations of various functions *)
open Printf
open Str

let set_password username password =
    let cmd = sprintf "/usr/bin/security add-generic-password -s LifeDB -a '%s' -p '%s'"
       (String.escaped username) (String.escaped password) in
    let ec,out,err = Fork_helper.system cmd [||] (Sys.getcwd ()) in
    match ec with
    |0 -> true
    |_ -> false


let get_password username = 
    let cmd = sprintf "/usr/bin/security find-generic-password -s LifeDB -a '%s' -g 2>&1| grep ^password | sed -e 's/password: \"//' -e 's/\"$//g'"
      (String.escaped username) in
    let ec,out,err = Fork_helper.system cmd [||] (Sys.getcwd ()) in
    match ec with
    |0 -> if out = "" then None else Some (List.hd (Str.split (Str.regexp_string "\n") out))
    |_ -> None
