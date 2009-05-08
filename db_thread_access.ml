(* This thread handles all the database write operations.
   There is a queue which represents "kick" requests from
   other threads. *)

open Utils
open Printf

type scan_request = [
   |`Plugins
   |`Lifedb
   |`Tasks
   |`Out_tasks
]

let q = Queue.create ()
let m = Mutex.create ()
let c = Condition.create ()

let string_of_scan_request = function
    |`Plugins -> "plugins"
    |`Lifedb -> "lifedb"
    |`Tasks -> "in_tasks"
    |`Out_tasks -> "out_tasks"

let dump_q () =
    printf "DB Queue: [";
    Queue.iter (fun (i,_) -> printf "%s " (string_of_scan_request i)) q;
    printf "]\n%!"

let push ?(copt=(None:Condition.t option)) (req:scan_request) =
    let pushit () =
        Queue.push (req,copt) q;
        Condition.signal c;
    in
    with_lock m (fun () ->
        (* push request if it's not a duplicate *)
        (try
            let top,_ = Queue.peek q in
            if top <> req then pushit ()
        with Queue.Empty -> pushit ());
    )

let tm = Mutex.create ()
let tb = ref 0
let tc = Condition.create ()

(* The calling thread will block until its ok to unthrottle, signalled
   by a signal on the tc Condition *)
let throttle_check () =
    with_lock tm (fun () ->
        if !tb > 0 then begin
          Log.logmod "Throttle" "throttle_check: blocking";
          Condition.wait tc tm;
          Log.logmod "Throttle" "throttle_check: woke up";
        end
    )

let throttle_request src fn =
   Log.logmod "Throttle" "throttle_request: from %s" src;
   with_lock tm (fun () -> incr tb);
   try_final fn
      (fun () ->
      with_lock tm (fun () ->
         decr tb;
         if !tb = 0 then
           Condition.signal tc;
      )
   )
