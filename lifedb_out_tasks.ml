(*pp $PP *)
(* Task scheduling *)

open Printf
open Utils
module LS=Lifedb_schema
module SS=Sync_schema

exception Task_error of string
exception Internal_task_error of string

let m = Mutex.create ()

type task_state = {
   cmd: string;
   plugin: string;
   mtype: string;
   cwd: string;
   start_time: float; 
   secret: (string * string) option;
   args : string list option;
   mutable outfd: Unix.file_descr option;
   mutable errfd: Unix.file_descr option;
   mutable running: Fork_helper.task;
   mutable uids: string list;
   mutable files: string list;
}

let task_list = Hashtbl.create 1
let task_table_limit = 20
let task_poll_period = ref 20.
let task_throttle () = Thread.delay 0.1

let json_of_task name t : Lifedb.Rpc.Task.out_r =
   let secret = match t.secret with |None -> None
      |Some (s,u) -> Some (object method service=s method username=u end) in
   object
       method name=name
       method plugin=t.plugin
       method pltype=t.mtype
       method secret=secret 
       method args=t.args
       method duration=Unix.gettimeofday () -. t.start_time
       method pid=Fork_helper.pid_of_task t.running
   end
 
let string_of_task t =
    let running = Fork_helper.string_of_task t.running in
    sprintf ": `%s` %s" t.cmd running

let log_task_table () = 
    Hashtbl.iter (fun name state ->
        Log.logmod "Tasks" "%s: %s" name (string_of_task state)
    ) task_list

let find_task name =
    try
       Some (Hashtbl.find task_list name)
    with
       Not_found -> None

let find_task_by_mtype mtype =
    let f = ref None in
    Hashtbl.iter (fun n t ->
      if t.mtype = mtype then
        f := Some (n,t)
    ) task_list;
    !f

(* create task descriptor and leave it unstarted *)
let create_task task_name (p:Lifedb.Rpc.Task.out_t)  =
    assert(not (Mutex.try_lock m));
    if Hashtbl.length task_list >= task_table_limit then
       raise (Task_error "too many tasks already registered");
    if String.contains task_name '.' || (String.contains task_name '/') then
       raise (Task_error "task name cant contain . or /");
    let pl = match Lifedb_plugin.find_plugin p#plugin with
      |None -> raise (Task_error (sprintf "plugin %s not found" p#plugin))
      |Some x -> x in
    let secret = match p#secret with 
      |None -> None
      |Some s -> Some (s#service, s#username) in
    let now_time = Unix.gettimeofday () in 
    let task = { cmd=pl#cmd; outfd=None; errfd=None; cwd=pl#dir; plugin=pl#name; secret=secret; start_time=now_time; running=(Fork_helper.blank_task ()); args=p#args; uids=[]; files=[]; mtype=p#pltype } in
    Hashtbl.add task_list task_name task;
    Log.logmod "Tasks" "Created outbound task '%s' %s" task_name (string_of_task task)

let find_or_create_task name (t:Lifedb.Rpc.Task.out_t) =
    match find_task name with
    |Some _ -> ()
    |None -> create_task name t

(* close any logging fds, reset fields *)
let reset_task name =
    assert(not (Mutex.try_lock m));
    let closeopt task = function
    |None -> ()
    |Some fd ->
      let lg = sprintf "[%s] Log closing: %s\n" (current_datetime()) (Fork_helper.string_of_task task.running) in
      ignore(Unix.handle_unix_error (Unix.write fd lg 0) (String.length lg));
      Unix.handle_unix_error Unix.close fd;
    in
    match find_task name with
    |Some task -> 
        let time_taken = (Unix.gettimeofday ()) -. task.start_time in
        let exit_code = Fork_helper.exit_code_of_task task.running in
        Log.push (`Plugin (name, time_taken, exit_code));
        closeopt task task.outfd;
        closeopt task task.errfd;
        task.outfd <- None;
        task.errfd <- None;
        task.running <- Fork_helper.blank_task ();
        task.uids <- [];
        task.files <- [];
    |None -> ()

(* remove the task entirely *)
let destroy_task name =
    assert(not (Mutex.try_lock m));
    match find_task name with
    |Some task -> begin
        let final_status = Fork_helper.destroy task.running in
        reset_task name;
        Hashtbl.remove task_list name;
        Log.logmod "Tasks" "Outbound task %s destroyed: %s" name 
            (Fork_helper.string_of_status final_status);
    end
    |None -> raise (Task_error "task not found")

(* split up a list of entris into a hashtable of their respective mtypes *)
let partition_entries_into_mtypes lifedb es =
  let h = Hashtbl.create 1 in
  List.iter (fun e ->
    if not (Hashtbl.mem h e#mtype#name) then
      Hashtbl.add h e#mtype#name [];
    Hashtbl.replace h e#mtype#name (e :: (Hashtbl.find h e#mtype#name))
  ) es;
  h

let start_task name =
  let t = Hashtbl.find task_list name in
  assert(not (Mutex.try_lock m));
  let env = match t.secret with 
  |None -> [||] 
  |Some (s,u) -> begin
    match Lifedb_passwd.lookup_passwd s u with
    |Some p -> [| ("LIFEDB_PASSWORD=" ^ p); ("LIFEDB_USERNAME="^u) |] 
    |None -> Log.logmod "Tasks" "WARNING: unable to find passwd for task '%s'" name; [||]
  end in
  (* add environment arguments *)
  let args = match t.args with None -> [||] | Some a -> Array.of_list a in
  let env = Array.append env args in
  let logdir = Lifedb_config.Dir.log() in
  let logfile = sprintf "%s/%s.log" logdir name in
  let errlogfile = sprintf "%s/%s.err" logdir name in
  let openfdfn f = Unix.handle_unix_error (Unix.openfile f [ Unix.O_APPEND; Unix.O_CREAT; Unix.O_WRONLY]) 0o600 in
  let outfd = openfdfn logfile in
  let errfd = openfdfn errlogfile in
  let logfn fd s = ignore(Unix.write fd s 0 (String.length s)) in
  let tmstr = current_datetime () in
  logfn outfd (sprintf "[%s] Stdout log started\n" tmstr);
  logfn errfd (sprintf "[%s] Stderr log started\n" tmstr);
  let env = Array.append env [| "LIFEDB_SYNC_DIR=out"; (sprintf "LIFEDB_UID_MAP=%s" (Lifedb_config.Dir.uidmap ()));
    (sprintf "HOME=%s" (Sys.getenv "HOME"));
    (sprintf "USER=%s" (Sys.getenv "USER")) |] in
  let cmd = 
    if Lifedb_config.test_mode () then
      sprintf "sleep %d" (Random.int 5 + 3)
    else
      (* XXX check shell escaping here!! *)
      sprintf "%s %s" t.cmd (String.concat " " (List.map String.escaped t.files))
  in
  let ts = Fork_helper.create cmd env t.cwd (logfn outfd) (logfn errfd) in
  Log.logmod "Tasks" "Executing outbound command '%s' (%s)" name cmd;
  task_throttle ();
  t.running <- ts;
  t.outfd <- Some outfd;
  t.errfd <- Some errfd

(* Look for items in the INBOX with a pltype matching an active plugin, and schedule it
   if so *)
let task_sweep lifedb syncdb () =
  (* for each user, look for entries in the inbox to them *)
  List.iter (fun (user:SS.User.t) ->
    let es = LS.Entry.get_by_inbox_delivered ~inbox:(Some user#uid) ~delivered:0L lifedb in
    match es with
    |[] -> ()
    |es -> begin
      (* constrain es to only 50 entries at a time to avoid overloading output plugin *)
      let es = list_max_size 50 es in
      (* we have inbox entries, look for a plugin to handle each mtype *)
      let h = partition_entries_into_mtypes lifedb es in
      Hashtbl.iter (fun mtype_name es ->
        (* look for an output task to handle this mtype name *)
        match find_task_by_mtype mtype_name with
        |None ->
          Log.logmod "Task" "Unable to find output task for <- %s : %s" mtype_name user#uid
        |Some (name,t) -> begin
          match Fork_helper.status_of_task t.running with
          |Fork_helper.Not_started ->
            (* set the entry UIDs and kick the command off *)
            t.uids <- List.map (fun e -> e#uid) es;
            t.files <- List.map (fun e -> e#file_name) es;
            start_task name
          |_ ->
            Log.logmod "Task" "Pending INBOX items, but already running %s" name
        end
      ) h
    end
  ) (SS.User.get syncdb);
  Hashtbl.iter (fun name task ->
    let td = string_of_task task in
    match Fork_helper.status_of_task task.running with
    |Fork_helper.Running pid ->
      Log.logmod "Sweep" "%s ... %s" name td
    |Fork_helper.Not_started -> ()
    |Fork_helper.Done exit_code ->
      Log.logmod "Sweep" "%s ... finished %s" name td;
      if exit_code = 0 then begin
        (* successfully delivered msgs, so mark them in the DB as delivered *)
        List.iter (fun uid ->
          match LS.Entry.get_by_uid ~uid lifedb with
          |[e] ->
            Log.logmod "Task" "Successfully delivered: %s" e#file_name;
            e#set_delivered 1L;
            ignore(e#save)
          |_ -> ()
        ) task.uids;
      end;
      reset_task name;
    |Fork_helper.Killed signal ->
      Log.logmod "Sweep" "%s ... crashed %s" name td;
      reset_task name;
  ) task_list

let dispatch cgi = function
   |`Create (name,p) ->
       let params = Lifedb.Rpc.Task.out_t_of_json (Json_io.json_of_string p) in
       with_lock m (fun () ->
           match find_task name with
           |Some state ->
               Lifedb_rpc.return_error cgi `Bad_request "Task already exists" "Use a different id"
           |None -> begin
               try
                 create_task name params
               with
               |Task_error err ->
                  Lifedb_rpc.return_error cgi `Bad_request "Task error" err
           end
       )
   |`Get name ->
       with_lock m (fun () ->
           match find_task name with
           |Some state ->
               cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Task.json_of_out_r (json_of_task name state)))
           |None ->
               Lifedb_rpc.return_error cgi `Not_found "Task error" "Task not found"
       )
   |`List ->
       with_lock m (fun () ->
           let r = Hashtbl.fold (fun name state a -> json_of_task name state :: a) task_list [] in
           let res = object method results=List.length r  method rows=r end in
           cgi#output#output_string (Json_io.string_of_json (Lifedb.Rpc.Task.json_of_out_rs res))
       )
   |`Destroy name ->
       with_lock m (fun () ->
           try
               destroy_task name
           with |Task_error err ->
               Lifedb_rpc.return_error cgi `Bad_request "Task error" err
       )

(* task thread which waits on a condition to do a sweep.  is signalled regularly
   or via a process exiting and delivering a SIGCHLD *)
let c = Condition.create ()
let cm = Mutex.create ()
let task_thread () =
  let lifedb = LS.Init.t (Lifedb_config.Dir.lifedb_db ()) in
  let syncdb = SS.Init.t (Lifedb_config.Dir.sync_db ()) in
  while true do
    with_lock cm (fun () ->
      Condition.wait c cm;
      with_lock m (task_sweep lifedb syncdb);
    )
  done

(* thread to kick the sweeping thread regularly to update task status. *)
let task_regular_kick () =
  while true do
    with_lock cm (fun () ->
      Condition.signal c
    );
    Thread.delay !task_poll_period
  done

(* scan the config directory and spawn tasks *)
let config_file_extension = ".outconf"
let scan_config_file config_file =
  Log.logmod "Tasks" "Scanning config file %s" config_file;
  let task = Lifedb.Rpc.Task.out_t_of_json (Json_io.load_json config_file) in
  let task_name = Filename.chop_suffix (Filename.basename config_file) config_file_extension  in
  match Lifedb_plugin.find_plugin task#plugin with
    |None -> Log.logmod "Tasks" "Plugin '%s' not found for task '%s', skipping it" task#plugin task_name;
    |Some _ ->
      Log.logmod "Tasks" "Added '%s' (plugin %s)" task_name task#plugin;
      let task : Lifedb.Rpc.Task.out_t = object
        method plugin=task#plugin
        method secret=task#secret
        method args=task#args
        method pltype=task#pltype
      end in
      with_lock m (fun () -> find_or_create_task task_name task)

let do_scan () =
    let config_dir = Lifedb_config.Dir.config () in
    let dh = Unix.opendir config_dir in
    try_final (fun () ->
        repeat_until_eof (fun () ->
           let next_entry = Unix.readdir dh in
           if Filename.check_suffix next_entry config_file_extension then
              scan_config_file (Filename.concat config_dir next_entry)
        )
    ) (fun () -> Unix.closedir dh)

let init () =
    let _ = Thread.create task_thread () in
    let _ = Thread.create task_regular_kick () in
    Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> 
        with_lock cm (fun () -> Condition.signal c)))
