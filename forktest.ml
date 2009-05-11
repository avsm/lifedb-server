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

open Fork_helper
open Printf

let _ = 
   let ofn = print_endline in
   let efn = print_endline in
   let t1 = create "find /Users/avsm/src/xgit" [||] "/" ofn efn in
   let t2  = create "ls -laR /Users/avsm/src/git" [||] "/" ofn efn in
   print_endline "done main";
   let () = match t1.thread with |Some t -> Thread.join t |None -> () in
   let () = match t2.thread with |Some t -> Thread.join t |None -> () in
   print_endline (string_of_task t1);
   print_endline (string_of_task t2);
   ()
   
