(*pp $PP *)

(* just some test functions at the moment *)
let m = Mutex.create ()

let n = ref [""]

let lockfn fn =
    Mutex.lock m;
    fn ();
    Mutex.unlock m

let get () =
    !n
    
let set x =
    n := x