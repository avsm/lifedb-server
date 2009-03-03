(*pp $PP *)
(* Task scheduling *)

open Printf
open Utils

exception Task_error of string
exception Internal_task_error of string

let m = Mutex.create ()

type json rpc_task_create = <
   name: string;
   cmd: string;
   mode: string;
   ?period: int option
>

type json rpc_task_destroy = <
   name: string
>

type json rpc_task = <
   cmd: string;
   mode: string;
   ?period: int option;
   ?pid: int option
>
and rpc_task_list = (string,rpc_task) Hashtbl.t

type task_mode = 
   |Periodic of int
   |Single 
   |Constant

type task_state = {
   cmd: string;
   mode: task_mode;
   start_time: float; 
   running: Fork_helper.task;
}

let task_list = Hashtbl.create 1
let task_table_limit = 10
let task_poll_period = ref 1.

let json_of_task t =
   let mode,period = match t.mode with
   |Periodic p -> "periodic", (Some p)
   |Single -> "single", None
   |Constant -> "constant", None in
   let pid = Fork_helper.pid_of_task t.running in
   object
       method cmd=t.cmd
       method mode=mode
       method period=period
       method pid=pid
   end
 
let string_of_task t =
    let running = Fork_helper.string_of_task t.running in
    let mode = match t.mode with 
    |Single -> "single"
    |Periodic p -> sprintf "periodic (every %d sec)" p
    |Constant -> "constant" in
    sprintf "(%s) : %s (%s)" mode t.cmd running

let log_task_table () = 
    Netplex_cenv.log `Info "logging task table...";
    Hashtbl.iter (fun name state ->
        Netplex_cenv.logf `Info "%s: %s" name (string_of_task state)
    ) task_list

let find_task name =
    try
       Some (Hashtbl.find task_list name)
    with
       Not_found -> None

let create_task params =
    if Hashtbl.length task_list >= task_table_limit then
       raise (Task_error "too many tasks already registered");
    let mode = match String.lowercase params#mode, params#period with
    |"periodic", (Some p) -> Periodic p
    |"single",_ -> Single
    |"constant",_ -> Constant
    |_,_ -> raise (Task_error "unknown task mode") in
    let outfn = print_endline in
    let errfn = print_endline in
    let task_status = Fork_helper.create params#cmd outfn errfn in
    let now_time = Unix.gettimeofday () in
    let task = { cmd=params#cmd; mode=mode; start_time=now_time; running=task_status } in
    Hashtbl.add task_list params#name task;
    Netplex_cenv.logf `Debug "Added task: %s" (string_of_task task)

let destroy_task name =
    match find_task name with
    |Some task -> begin
        let final_status = Fork_helper.destroy task.running in
        Hashtbl.remove task_list name;
        Netplex_cenv.logf `Info "Task %s destroyed: %s" name 
            (Fork_helper.string_of_status final_status);
    end
    |None -> raise (Task_error "task not found")

let reschedule_task c name task =
    match task.mode with 
    |Single -> ()
    |Constant ->
         c#log `Debug (sprintf "restarting %s (constant)" name);
         let task_status = Fork_helper.create task.cmd print_endline print_endline in
         let now_time = Unix.gettimeofday () in
         let task = { cmd=task.cmd; mode=Constant; start_time=now_time; running=task_status } in
         Hashtbl.add task_list name task
    |Periodic p ->
         let start_time = Unix.gettimeofday () +. (float p) in
         let task_status = Fork_helper.blank_task () in
         let task = { cmd=task.cmd; mode=Periodic p; start_time=start_time; running=task_status } in
         Hashtbl.add task_list name task;
         c#log`Debug (sprintf "scheduling %s: %s" name (Fork_helper.string_of_task task_status))

let task_sweep c =
    Hashtbl.iter (fun name task ->
       let td = Fork_helper.string_of_task task.running in
       match Fork_helper.status_of_task task.running with
       |Fork_helper.Running pid ->
           c#log `Debug (sprintf "sweep: %s" td)
       |Fork_helper.Not_started ->
           let curtime = Unix.gettimeofday () in
           if task.start_time < curtime then begin
               let task_status = Fork_helper.create task.cmd print_endline print_endline in
               let task = { task with start_time=curtime; running=task_status } in
               Hashtbl.replace task_list name task
           end
       |Fork_helper.Done exit_code ->
           c#log `Debug (sprintf "sweep: finished %s" td);
           Hashtbl.remove task_list name;
           reschedule_task c name task;
       |Fork_helper.Killed signal ->
           c#log `Debug (sprintf "sweep: crashed %s" td);
           Hashtbl.remove task_list name;
           reschedule_task c name task;
    ) task_list

let dispatch cgi = function
   |`Create p ->
       let params = rpc_task_create_of_json (Json_io.json_of_string p) in
       with_lock m (fun () ->
           match find_task params#name with
           |Some state ->
               Lifedb_rpc.return_error cgi `Bad_request "Task already exists" "Use a different id"
           |None -> begin
               try
                 create_task params
               with
               |Task_error err ->
                  Lifedb_rpc.return_error cgi `Bad_request "Task error" err
           end
       )
   |`Get name ->
       with_lock m (fun () ->
           match find_task name with
           |Some state ->
               cgi#output#output_string (Json_io.string_of_json (json_of_rpc_task (json_of_task state)))
           |None ->
               Lifedb_rpc.return_error cgi `Not_found "Task error" "Task not found"
       )
   |`List ->
       with_lock m (fun () ->
           let resp = Hashtbl.create 1 in
           Hashtbl.iter (fun name state -> Hashtbl.add resp name (json_of_task state)) task_list;
           cgi#output#output_string (Json_io.string_of_json (json_of_rpc_task_list resp))
       )
   |`Destroy p ->
       let params = rpc_task_destroy_of_json (Json_io.json_of_string p) in
       with_lock m (fun () ->
           try
               destroy_task params#name
           with |Task_error err ->
               Lifedb_rpc.return_error cgi `Bad_request "Task error" err
       )

let singleton () =
    let hooks = object
        inherit Netplex_kit.empty_processor_hooks() as super

        val mutable signal_stop = true

        method post_start_hook c =
            super#post_start_hook c;
            ignore(Thread.create (fun () ->
                while signal_stop do
                    with_lock m (fun () -> task_sweep c);
                    Thread.delay !task_poll_period;
                done;
                c#log `Debug "Terminating task thread"
            ) ())
            
        method receive_admin_message c msg args =
            c#log `Info (sprintf "received admin msg %s [%s]" msg (String.concat "," (Array.to_list args)));
            match msg,args with
            |"tasks",[|"dump"|] ->
                c#log `Debug "Dumping tasks table";
                with_lock m log_task_table
            |_ -> ()
            
        method shutdown () =
            signal_stop <- false;
            super#shutdown ()
    end in

    object (self)
        method name = "tasks"
        method create_processor _ _ _ =
            object (self)
            inherit Netplex_kit.processor_base hooks
            method process ~when_done _ _ _ = when_done () (* should not ever be called *)
            method supported_ptypes = [ `Multi_threading ]
        end
    end
