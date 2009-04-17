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
let tb = ref false

let throttle_check () =
    let sl = with_lock tm (fun () ->
      if !tb then (tb := false; true) else false
    ) in
    if sl then (
      Log.logmod "Throttle" "sleeping...";
      Thread.delay 10.;
    )

let throttle_request () =
   with_lock tm (fun () ->
      tb := true
   )

let push_sync req =
    let c' = Condition.create () in
    let m' = Mutex.create () in
    push ~copt:(Some c') req;
    Condition.wait c' m'
