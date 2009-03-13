(* Platform-specific implementations of various functions *)
open Printf

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
    |0 -> if out = "" then None else Some out
    |_ -> None
