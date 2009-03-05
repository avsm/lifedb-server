(*pp $PP *)

(* This thread handles all the database write operations.
   There is a queue which represents "kick" requests from
   other threads. *)

open Utils
open Printf

type scan_request = 
   |Plugins
   |Lifedb

let q = Queue.create ()
let m = Mutex.create ()
let c = Condition.create ()

let string_of_scan_request = function
    |Plugins -> "plugins"
    |Lifedb -> "lifedb"

let dump_q () =
    printf "Queue: [";
    Queue.iter (fun i -> printf "%s " (string_of_scan_request i)) q;
    printf "]\n%!"
    
let push req =
    printf "QUEUE: push req started: %s\n%!" (string_of_scan_request req);
    let pushit () =
        print_endline (sprintf "pushing %s to queue" 
            (string_of_scan_request req));
        Queue.push req q;
        dump_q();
        Condition.signal c;
    in
    with_lock m (fun () ->
        (* push request if it's not a duplicate *)
        (try
            let top = Queue.peek q in
            if top <> req then pushit ()
        with Queue.Empty -> pushit ());
    )

