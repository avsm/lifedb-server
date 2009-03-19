open Unix
open Printf

type status =
  |Not_started
  |Running of int
  |Done of int
  |Killed of int

type task = {
  thread: Thread.t option;
  status: status ref;
}
  
let string_of_status = function
  |Not_started -> "not started"
  |Running pid -> sprintf "running (pid %d)" pid
  |Done exit_code -> sprintf "done (exit code %d)" exit_code
  |Killed signal -> sprintf "killed (signal %d)" signal

let pid_of_status = function 
  |Running pid -> Some pid
  |_ -> None

let string_of_task t = string_of_status !(t.status)
let pid_of_task t = pid_of_status !(t.status)
let status_of_task t = !(t.status)
let exit_code_of_task t = match !(t.status) with
    |Done e -> e |_ -> -1

let blank_task () = { thread=None; status=(ref Not_started) }

let fork_and_connect_fds args env cwd =
   let pin_r, pin_w = pipe () in
   let pout_r, pout_w = pipe () in
   let perr_r, perr_w = pipe () in 
   match fork () with
   |0 -> (* child process *)
       let dup2_and_close f1 f2 = dup2 f1 f2; close f1 in
       close pin_w;
       dup2_and_close pin_r stdin;
       close pout_r;
       dup2_and_close pout_w stdout;
       close perr_r;
       dup2_and_close perr_w stderr;
       handle_unix_error chdir cwd;
       handle_unix_error (execvpe args.(0) args) env; 
   |pid -> (* parent process *)
       List.iter close [pin_r; pout_w; perr_w];
       pid, pin_w, pout_r, perr_r

let fork_and_read args env cwd status outfn errfn =
   let pid, stdin, stdout, stderr = fork_and_connect_fds args env cwd in
   status := Running pid;
   close stdin;
   let buflen = 1024 in
   let fds = ref [stdout; stderr] in
   let buf = String.create buflen in
   let readfn fn fd =
      match Unix.read fd buf 0 (String.length buf) with
      |0|(-1) -> fds := List.filter (fun f -> f <> fd) !fds; Unix.close fd
      |r when r=(String.length buf) -> fn buf
      |r ->  fn (String.sub buf 0 r)
   in
   let stdoutfn = readfn outfn in
   let stderrfn = readfn errfn in
   while List.length !fds > 0 do
      match Unix.select !fds [] !fds (-1.0) with
      |[],_,[] -> ()
      |x::y,_,_ ->
         List.iter (function
            |fd when fd=stdout -> stdoutfn fd
            |fd when fd=stderr -> stderrfn fd
            |_ -> ()
         ) (x::y)
      |[],_,x ->
         List.iter (fun f ->
            fds := List.filter (fun f' -> f' <> f) !fds
          ) x
   done;
   (try close stdout with _ -> ());
   (try close stderr with _ -> ());
   match Unix.waitpid [] pid with
   |_,Unix.WEXITED status_code -> status := Done status_code
   |_,Unix.WSIGNALED signal -> status := Killed signal
   |_,Unix.WSTOPPED _ -> ()

let create cmd env cwd outfn errfn =
   let args = [| "/bin/sh"; "-c"; cmd |] in
   let status = ref Not_started in
   let t = Thread.create (fork_and_read args env cwd status outfn) errfn in
   { status = status; thread = Some t }

let destroy t =
   let _ = match !(t.status) with
   |Running pid -> kill pid Sys.sigterm
   |_ -> () in
   (* let () = match t.thread with |Some t -> Thread.join t |None -> () in *)
   !(t.status)

let system cmd env cwd =
   let ob = Buffer.create 1 in
   let eb = Buffer.create 1 in
   let ofn = Buffer.add_string ob in
   let efn = Buffer.add_string eb in
   let t = create cmd env cwd ofn efn in
   let () = match t.thread with
   |Some th -> Thread.join th
   |None -> assert false in
   let os = Buffer.contents ob in
   let es = Buffer.contents eb in
   let ec = exit_code_of_task t in
   ec,os,es
