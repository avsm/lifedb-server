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
   
