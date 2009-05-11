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

open Platform_macos
open Printf

let _ = 
   (if set_password "floozie" "wibbles" then
       printf "set_password: true\n"
   else
       printf "set_password: FAIL\n");
   (match get_password "floozie" with
   |Some x -> printf "get_password: %s\n" x
   |None -> printf "get_password: FAIL\n");
   print_endline "done"
   
