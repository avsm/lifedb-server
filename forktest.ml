open Fork_helper
open Printf

let _ = 
   let ofn = print_endline in
   let efn = print_endline in
   let t1 = create "find /Users/avsm/src/xgit" ofn efn in
   let t2  = create "ls -laR /Users/avsm/src/git" ofn efn in
   print_endline "done main";
   Thread.join t1.thread;
   Thread.join t2.thread;
   print_endline (string_of_task t1);
   print_endline (string_of_task t2);
   ()
   
