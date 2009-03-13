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
   
